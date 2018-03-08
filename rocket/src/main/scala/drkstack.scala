
package rocket



import Chisel._
import uncore._
import junctions._
import cde.{Parameters, Field}
import Util._

trait HasDRKStackParameters{ 
  //drkStackSize has to be power of 2
  val drkStackSize = 128
  val drkStackAddrBits = log2Up(drkStackSize)
  val drkStackNeedWB = 17
  val drkStackNeedFetch = 32
  val drkStackShiftAmt = 8
  require(drkStackShiftAmt == 8)

  def addrToAddr(addr_in: UInt): UInt = addr_in(drkStackAddrBits-1,log2Up(drkStackShiftAmt))
  def addrToOffset(addr_in: UInt): UInt = addr_in(log2Up(drkStackShiftAmt)-1,0)
  def addrToMask(addr_in: UInt): UInt = UIntToOH(addrToOffset(addr_in))

  val debugRefiller = true
  val debugStackWrapper = true
  val debugTop = true
}


object DRConsts {

  val stack_cmd_none :: stack_cmd_push :: stack_cmd_pop :: stack_cmd_stop :: stack_cmd_start :: stack_cmd_out :: stack_cmd_in :: stack_cmd_mmgl :: stack_cmd_mmll :: stack_cmd_stack_dbg_set :: stack_cmd_stack_dbg_get :: stack_cmd_start_xoring:: Nil  = Enum(UInt(),12)

}

class DRKStackWrapperReq(implicit p:Parameters) extends DRBundle()(p)
with HasDRKStackParameters{
      val write = Bool(OUTPUT)
      val addr = UInt(OUTPUT,width=drkStackAddrBits)
      val id = UInt(OUTPUT,width=2)
      val wide = Bool(OUTPUT)
      val wdata = UInt(OUTPUT,width=drKeyBits)
      val wdata_wide = Vec(drkStackShiftAmt,UInt(OUTPUT,width=drKeyBits))
}

class DRKStackWrapperResp(implicit p:Parameters) extends DRBundle()(p)
with HasDRKStackParameters{
      val id = UInt(OUTPUT)
      val rdata = UInt(OUTPUT)
      val rdata_wide = Vec(drkStackShiftAmt,UInt(OUTPUT,width=drKeyBits))
      val valid = Bool(OUTPUT)
}



  

class DRKStackTopCMDIO extends Bundle {

  val op = UInt(OUTPUT,8)
  val data20 = UInt(OUTPUT,20)
  val key = UInt(OUTPUT,64)
}
class DRKConfigIO extends Bundle {
    val next_base = UInt(OUTPUT)
    val cur_base = UInt(OUTPUT)
    val prev_base = UInt(OUTPUT)
    val stack_region_base = UInt(OUTPUT)
}
class DRKStatusIO extends Bundle{
      val active = Bool(OUTPUT)
      val next_base = UInt(OUTPUT) 
      val cur_base = UInt(OUTPUT) 
      val prev_base = UInt(OUTPUT) 
      val stack_region_base = UInt(OUTPUT)
      val stack_dbg_get_data = UInt(OUTPUT)

    }
class DRKStackTop(implicit p: Parameters) extends DRModule()(p)
with HasDRKStackParameters{
  val init_printed = Reg(init=Bool(false))
  val io = new Bundle{
    
    val config = (new DRKConfigIO).flip
    val status = new DRKStatusIO

    val perf_stack_top = UInt(OUTPUT)
    val perf_wb_internal = Bool(OUTPUT)
    val perf_fetch_internal = Bool(OUTPUT)

    val status_core = new Bundle{
      val cannot_pop = Bool(OUTPUT)
      val cannot_push = Bool(OUTPUT)
      val handling_pop = Bool(OUTPUT)
      val handling_push = Bool(OUTPUT)
      val dry_xoring = Bool(OUTPUT)
      val need_wb = Bool(OUTPUT)
    }
    val cmd = Decoupled{
  
      new DRKStackTopCMDIO
    }.flip

    //now Decoupled
    val rf_req = Decoupled(new Bundle{
      val id = UInt(OUTPUT)
      }
    ).flip
    val rf_resp = Decoupled(new Bundle{
      val key = UInt(OUTPUT)
    })

    val mem = new ClientUncachedTileLinkIO
  }//io
  //body begins

  //submodules
  val the_stack = Module(new DRKStackWrapper()(p))
  val refiller = Module(new DRKStackRefiller()(p))
  val stack_req_arb = Module(new Arbiter(new DRKStackWrapperReq()(p),3))
  
  //trivial wiring
  the_stack.io.pipe_in <> stack_req_arb.io.out
  io.mem <> refiller.io.mem



  val rf_stack_top = Reg(init=UInt(0,width=drkStackAddrBits))
  val rf_stack_bottom = UInt(0,width=drkStackAddrBits)
  val rf_stack_offset = Reg(init=UInt(0,width=drkStackAddrBits))


  //conifg/status
  val active = Reg(init=Bool(false))
  val dry_xoring = Reg(init=Bool(false))
  val reg_stack_region_base = Reg(init=UInt(0,width=paddrBits)) 
  val reg_addr_next_base = Reg(init=UInt(0,width=paddrBits))
  val reg_addr_cur_base = Reg(init=UInt(0,width=paddrBits))
  val reg_addr_prev_base = Reg(init=UInt(0,width=paddrBits))
  io.status.active := active
  io.status_core.dry_xoring := dry_xoring
  io.status.stack_region_base := reg_stack_region_base
  io.status.next_base := reg_addr_next_base
  io.status.cur_base := reg_addr_cur_base
  io.status.prev_base := reg_addr_prev_base


  //internal
  val reg_prev_num_ids = Reg(init=UInt(0,width=drkStackAddrBits))
  val reg_stack_region_size = Reg(init=UInt(0,width=paddrBits)) 
  val reg_stack_cached_base = Reg(init=UInt(0,width=paddrBits))
  io.perf_stack_top := reg_stack_cached_base + (rf_stack_top << 3) - reg_stack_region_base 
 
  //derived combinational
  val offset_next_base = reg_addr_next_base - reg_stack_cached_base
  val offset_cur_base = reg_addr_cur_base - reg_stack_cached_base
  val offset_prev_base = reg_addr_prev_base - reg_stack_cached_base
  val pt_next_base = Wire(init=UInt(0,width=drkStackAddrBits))
  pt_next_base := (rf_stack_bottom + (offset_next_base >> 3))(drkStackAddrBits-1,0)
  val pt_cur_base = Wire(init=UInt(0,width=drkStackAddrBits))
  pt_cur_base := (rf_stack_bottom + (offset_cur_base >> 3))(drkStackAddrBits-1,0)

  val pt_prev_base = Wire(init=UInt(0,width=drkStackAddrBits))
  pt_prev_base := (rf_stack_bottom + (offset_prev_base >> 3))(drkStackAddrBits-1,0)


  val reg_stack_dbg_base = Reg(init=UInt(0))
  val reg_stack_dbg_offset = Reg(init=UInt(0))


  //type 1 commands. could be burst
  val is_mmxl = (io.cmd.bits.op === DRConsts.stack_cmd_mmgl ||
		io.cmd.bits.op === DRConsts.stack_cmd_mmll)
  val stack_dbg_offset = (io.cmd.bits.data20(8,0))
  val stack_dbg_base = Mux(io.cmd.bits.data20(10,9) === UInt(0x2),pt_next_base,
                                Mux(io.cmd.bits.data20(10,9) === UInt(0x1),pt_cur_base,
                                pt_prev_base))
  val dbg_addr = stack_dbg_base + stack_dbg_offset
  stack_req_arb.io.in(0).valid := io.cmd.valid && 
  				(io.cmd.bits.op === DRConsts.stack_cmd_stack_dbg_set ||
				io.cmd.bits.op === DRConsts.stack_cmd_mmgl ||
				io.cmd.bits.op === DRConsts.stack_cmd_mmll)
  stack_req_arb.io.in(0).bits.write := is_mmxl
  stack_req_arb.io.in(0).bits.addr := Mux(is_mmxl,pt_next_base + UInt(1) + getMMDest(io.cmd.bits.data20), dbg_addr) + rf_stack_offset
  stack_req_arb.io.in(0).bits.id := UInt(0)
  stack_req_arb.io.in(0).bits.wide := Bool(false)
  stack_req_arb.io.in(0).bits.wdata := io.cmd.bits.key
  stack_req_arb.io.in(0).bits.wdata_wide := Vec(drkStackShiftAmt,UInt(0xFACE))
  io.cmd.ready := stack_req_arb.io.in(0).ready 

  val reg_dbg_get_data = Reg(UInt())
  val reg_dbg_get_ready = Reg(init=Bool(false))

  when(the_stack.io.pipe_out.valid && reg_dbg_get_ready){
    reg_dbg_get_data := the_stack.io.pipe_out.rdata
    reg_dbg_get_ready := Bool(false)
  }.otherwise{
    reg_dbg_get_ready := io.cmd.fire() && io.cmd.bits.op === DRConsts.stack_cmd_stack_dbg_set
  }
  io.status.stack_dbg_get_data := reg_dbg_get_data

  when(active){
    when(io.cmd.fire()){ 
      when(io.cmd.bits.op === DRConsts.stack_cmd_mmgl){ 
        //rf_stack(pt_next_base + UInt(1) + getMMDest(io.cmd.bits.data20)) := 
        //  io.cmd.bits.key
        when(getMMDest(io.cmd.bits.data20) + UInt(2) + pt_next_base > rf_stack_top){
          rf_stack_top := getMMDest(io.cmd.bits.data20) + UInt(2) + pt_next_base
        }
      }
      when(io.cmd.bits.op === DRConsts.stack_cmd_mmll){
        //rf_stack(pt_next_base + UInt(1) + getMMDest(io.cmd.bits.data20)) := 
        //  rf_stack(pt_cur_base + UInt(1) + getMMSrc(io.cmd.bits.data20))
        when(getMMDest(io.cmd.bits.data20) + UInt(2) + pt_next_base > rf_stack_top){
          rf_stack_top := getMMDest(io.cmd.bits.data20) + UInt(2) + pt_next_base
        }
      }
    }
  }


  //type1 handled

  //type 2 commands and wb/fetch

  val ms_ready :: ms_push :: ms_pop ::ms_pop_2:: ms_wb_wait :: ms_wb_valid :: ms_fetch :: ms_fetch_valid :: ms_check :: ms_check_2 :: Nil = Enum(UInt(),10)
  val ms = Reg(init=ms_ready)

  val need_wb = (UInt(drkStackSize) - UInt(1) - rf_stack_top < UInt(drkStackNeedWB))
  val need_fetch = ( (pt_prev_base < UInt(0x11)) ||  (rf_stack_top < UInt(drkStackNeedFetch))) &&
			(reg_stack_cached_base =/= reg_stack_region_base)

  io.status_core.cannot_pop :=  ms =/= ms_ready
  io.status_core.cannot_push :=  ms =/= ms_ready
  io.status_core.handling_pop := (ms === ms_check)
  io.status_core.handling_push := (ms === ms_check) 
  io.status_core.need_wb := need_wb
  val ms_reg_addr = Reg(init=UInt(0))
  val ms_reg_wdata = Reg(init=UInt(0))
  val ms_reg_rdata_wide = Reg(init=Vec.fill(drkStackShiftAmt){UInt(0,width=drKeyBits)})
  val ms_reg_rdata_wide_valid = Reg(init=Bool(false))
  val ms_reg_stack_cached_base = Reg(UInt())
  val ms_reg_wdata_wide = Reg(init=Vec.fill(drkStackShiftAmt){UInt(0,width=drKeyBits)})
  val ms_reg_new_prev_base_valid = Reg(init=Bool(false))
  stack_req_arb.io.in(1).valid := ms === ms_push || ms === ms_pop || ms === ms_wb_wait || ms === ms_fetch_valid
  stack_req_arb.io.in(1).bits.write := ms === ms_push || ms === ms_fetch_valid
  stack_req_arb.io.in(1).bits.addr := ms_reg_addr + rf_stack_offset
  stack_req_arb.io.in(1).bits.id := UInt(1)
  stack_req_arb.io.in(1).bits.wide := ms === ms_wb_wait || ms === ms_fetch_valid
  stack_req_arb.io.in(1).bits.wdata := ms_reg_wdata
  stack_req_arb.io.in(1).bits.wdata_wide := ms_reg_wdata_wide

  ms_reg_rdata_wide := Mux(ms === ms_wb_valid && ms_reg_rdata_wide_valid === Bool(false), the_stack.io.pipe_out.rdata_wide, ms_reg_rdata_wide)
	
  refiller.io.wb.bits.stack_cached_base := ms_reg_stack_cached_base
  refiller.io.wb.bits.keys := ms_reg_rdata_wide		
  refiller.io.wb.valid := ms_reg_rdata_wide_valid && ms === ms_wb_valid

  refiller.io.fetch.valid := ms === ms_fetch
  refiller.io.fetch.bits.stack_cached_base := reg_stack_cached_base
  io.perf_wb_internal := refiller.io.wb.fire()
  io.perf_fetch_internal := refiller.io.fetch.fire()


  when(active){
    when(ms === ms_pop_2){
      reg_addr_prev_base := reg_addr_prev_base - ((the_stack.io.pipe_out.rdata + UInt(1)) << 3)
    }.elsewhen(ms === ms_push && stack_req_arb.io.in(1).fire()){
      reg_addr_prev_base := reg_addr_cur_base 
    }
  }

  when(active){
    when(ms === ms_ready){
      when(io.cmd.fire() && io.cmd.bits.op === DRConsts.stack_cmd_push){
	ms := ms_push
	ms_reg_addr := pt_next_base
	ms_reg_wdata := io.cmd.bits.data20
      }.elsewhen(io.cmd.fire() && io.cmd.bits.op === DRConsts.stack_cmd_pop){
        ms := ms_pop
	ms_reg_addr := pt_prev_base
      }
/*
       .elsewhen(need_wb){
        ms_reg_rdata_wide_valid := Bool(false)
	ms_reg_addr := UInt(0) 
	ms := ms_wb_wait
      }.elsewhen(need_fetch){
        printf("need_fetch at ready\n")

        ms := ms_fetch 
	ms_reg_addr := UInt(drkStackSize - drkStackShiftAmt) 
      }*/
    }.elsewhen(ms === ms_push){
      when(stack_req_arb.io.in(1).fire()){
        ms := ms_check
        reg_addr_cur_base := reg_addr_next_base
        reg_addr_next_base := Cat(rf_stack_top,UInt(0,width=3)) + reg_stack_cached_base
        //rf_stack(pt_next_base) := io.cmd.bits.data20 
        rf_stack_top := rf_stack_top + UInt(1)
      }
    }.elsewhen(ms === ms_pop){
      when(stack_req_arb.io.in(1).fire()){
        ms := ms_pop_2
        //reg_addr_prev_base := reg_addr_prev_base - ((rf_stack(pt_prev_base) + UInt(1)) << 3)
        reg_addr_cur_base := reg_addr_prev_base
        reg_addr_next_base := reg_addr_cur_base 
        rf_stack_top := pt_cur_base + UInt(1)//rf_stack_bottom + ((reg_addr_cur_base - reg_stack_cached_base) >> 3) + UInt(1) 
      }
    }.elsewhen(ms === ms_pop_2){
      ms := ms_check
    }.elsewhen(ms === ms_check){
      when(need_wb){
        ms_reg_rdata_wide_valid := Bool(false)
	ms := ms_wb_wait
	ms_reg_addr := UInt(0)
      }.elsewhen(need_fetch){
        printf("need_fetch at check\n")
        ms := ms_fetch
	ms_reg_addr := UInt(drkStackSize - drkStackShiftAmt) 
      }.otherwise{
        ms := ms_ready
      }
    }.elsewhen(ms === ms_wb_valid){

      when(the_stack.io.pipe_out.valid && the_stack.io.pipe_out.id === UInt(1)){ 
	ms_reg_rdata_wide_valid := Bool(true)
      }
      when(refiller.io.wb.fire()){
	ms_reg_rdata_wide_valid := Bool(false)
        ms := ms_check
      }
    }.elsewhen(ms === ms_wb_wait){
      when(stack_req_arb.io.in(1).fire()){
        ms_reg_stack_cached_base := reg_stack_cached_base
        reg_stack_cached_base := reg_stack_cached_base + Cat(UInt(drkStackShiftAmt),UInt(0,width=3))
        when(rf_stack_offset === drkStackSize - drkStackShiftAmt){
          rf_stack_offset := UInt(0)
        }.otherwise{
          rf_stack_offset := rf_stack_offset + UInt(drkStackShiftAmt)
        }
        rf_stack_top := rf_stack_top - UInt(drkStackShiftAmt)

	ms_reg_rdata_wide_valid := Bool(false)
	ms := ms_wb_valid
      }
    }.elsewhen(ms === ms_fetch){
      when(refiller.io.fetch.fire()){
        ms_reg_wdata_wide := refiller.io.fetch_resp.keys
	ms := ms_fetch_valid
      }
    }.elsewhen(ms === ms_fetch_valid){
      when(stack_req_arb.io.in(1).fire()){

        reg_stack_cached_base := reg_stack_cached_base - Cat(UInt(drkStackShiftAmt),UInt(0,width=3))
        /*
        for(i <- 0 until drkStackSize - drkStackShiftAmt){
          rf_stack(i+ drkStackShiftAmt) := rf_stack(i)
        }
        for(i <-0 until drkStackShiftAmt){
          rf_stack(i) := refiller.io.fetch_resp.bits.keys(i)
        }*/
        when(rf_stack_offset === UInt(0)){
          rf_stack_offset := drkStackSize - drkStackShiftAmt
        }.otherwise{
          rf_stack_offset := rf_stack_offset - UInt(drkStackShiftAmt)
        }
  
        rf_stack_top := rf_stack_top + UInt(8)
	ms := ms_check
      }
    }
  }.elsewhen(ms === ms_check_2){
     //now may need more then one wb/fetch
     when(need_wb){
      ms_reg_rdata_wide_valid := Bool(false)
      ms := ms_wb_wait
      ms_reg_addr := UInt(0)
    }.elsewhen(need_fetch){
      printf("need_fetch at check\n")
      ms := ms_fetch
      ms_reg_addr := UInt(drkStackSize - drkStackShiftAmt) 
    }.otherwise{
      ms := ms_ready
    }
  }

  //type 2 cmd handled

  //dyn_key fetching being
  io.rf_req.ready := stack_req_arb.io.in(2).ready
  stack_req_arb.io.in(2).valid := io.rf_req.valid
  stack_req_arb.io.in(2).bits.write := Bool(false)
  stack_req_arb.io.in(2).bits.addr := pt_cur_base + UInt(1) + io.rf_req.bits.id + rf_stack_offset
  stack_req_arb.io.in(2).bits.id := UInt(2)
  io.rf_resp.bits.key := the_stack.io.pipe_out.rdata
  io.rf_resp.valid := the_stack.io.pipe_out.valid && the_stack.io.pipe_out.id === UInt(2)
  //dyn_key fetching end

  

  when(active === Bool(false)){
  //config mode   
     reg_addr_prev_base := io.config.prev_base
     reg_addr_cur_base := io.config.cur_base
     reg_addr_next_base := io.config.next_base
     reg_stack_region_base := io.config.stack_region_base
     reg_stack_cached_base := io.config.stack_region_base
     rf_stack_top := ((io.config.next_base - io.config.stack_region_base) >> 3) + UInt(1)
     when(io.cmd.fire() && io.cmd.bits.op === DRConsts.stack_cmd_start){
       active := Bool(true)
       dry_xoring := Bool(true)
     }.elsewhen(io.cmd.fire() && io.cmd.bits.op === DRConsts.stack_cmd_start_xoring){
       active := Bool(true)
       dry_xoring := Bool(false)
     }

  }.otherwise{
    when(io.cmd.bits.op === DRConsts.stack_cmd_stop){
      active := Bool(false)
      dry_xoring := Bool(false)
    }
  }




  if(debugDRKStack && debugTop){
    printf("\n\n=====Status in DRKStack(begin)=====\n")
      printf("\tio.cmd.ready:\t%x\n",io.cmd.ready)
      printf("\tio.status.stack_dbg_get_data:\t%x\n",io.status.stack_dbg_get_data)
      printf("\tio.status_core.cannot_pop:\t%x\n",io.status_core.cannot_pop)
      printf("\tio.status_core.cannot_push:\t%x\n",io.status_core.cannot_push)
      when(io.rf_req.fire()){
        printf("rf_req fires with id:\t%x\n",io.rf_req.bits.id)
      }
      when(io.rf_resp.fire()){
        printf("rf_resp fires with key:\t%x\n",io.rf_resp.bits.key)
      }
      printf("\trf_stack_top:\t%x\n",rf_stack_top)
      printf("\tpt_next_base:\t%x\n",pt_next_base)
      printf("\tpt_cur_base:\t%x\n",pt_cur_base)
      printf("\tpt_prev_base:\t%x\n",pt_prev_base)
      printf("\treg_stack_cached_base:\t%x\n",reg_stack_cached_base)
      printf("\treg_stack_region_base:\t%x\n",reg_stack_region_base)
      printf("\treg_addr_next_base:\t%x\n",reg_addr_next_base)
      printf("\treg_addr_cur_base:\t%x\n",reg_addr_cur_base)
      printf("\treg_addr_prev_base:\t%x\n",reg_addr_prev_base)
      printf("\tneed_fetch:\t%x\n",need_fetch)
      printf("\tneed_wb:\t%x\n",need_wb)
      printf("\tthe_stack.io.pipe_out.rdata:\t%x\n",the_stack.io.pipe_out.rdata)
      printf("\tthe_stack.io.pipe_out.id:\t%x\n",the_stack.io.pipe_out.id)
      printf("\tthe_stack.io.pipe_out.valid:\t%x\n",the_stack.io.pipe_out.valid)
      printf("\tms:\t%x\n",ms)

      when(io.cmd.fire()){
        printf("\tio.cmd.fire()\n")
          printf("\t\top:\t%x\n",io.cmd.bits.op)
          printf("\t\tdata20:\t%x\n",io.cmd.bits.data20)
          printf("\t\tkey:\t%x\n",io.cmd.bits.key)
          when(io.cmd.bits.op === DRConsts.stack_cmd_mmgl){
            printf("\t\t\top === mmgl(%x)\n",DRConsts.stack_cmd_mmgl)
              printf("\t\t\tdyn_id:\t%x\n",getMMDest(io.cmd.bits.data20))
              printf("\t\t\tpt_next_base:\t%x\n",pt_next_base)
              printf("\t\t\twriting to:\t%x\n",pt_next_base + UInt(1) + getMMDest(io.cmd.bits.data20))
              printf("\t\t\tincoming key:\t%x\n",io.cmd.bits.key)
          }
        when(io.cmd.bits.op === DRConsts.stack_cmd_pop){
          printf("\t\t\top === pop(%x)\n",DRConsts.stack_cmd_pop)
            printf("\t\t\treg_addr_prev_base:\t%x\n",reg_addr_prev_base)
            //printf("\t\t\tlength of prev (rf_stack(pt_prev_base)):\t%x\n",rf_stack(pt_prev_base))
            //printf("\t\t\tnew prev:\t%x\n",reg_addr_prev_base - ((rf_stack(addrToAddr(pt_prev_base + rf_stack_offset)(addrToOffset(pt_prev_base + rf_stack_offset)) + UInt(1)) << 3))
        }

        when(io.cmd.bits.op === DRConsts.stack_cmd_push){
          printf("\t\t\top === push(%x)\n",DRConsts.stack_cmd_push)
            //printf("\t\t\tlength of prev (rf_stack(pt_prev_base)):\t%x\n",rf_stack(pt_prev_base))
            //printf("\t\t\tnew prev:\t%x\n",reg_addr_prev_base - ((rf_stack(addrToAddr(pt_prev_base + rf_stack_offset)(addrToOffset(pt_prev_base + rf_stack_offset)) + UInt(1)) << 3))
        }
      }
    printf("\t\trefiller.io.wb.valid:\t%x\n",refiller.io.wb.valid) 
    when(refiller.io.wb.fire()){
      printf("\t\trefiller.io.wb.fire() with keys:\n")
      for(i <- 0 until drkStackShiftAmt){
        printf("\t\t\t%d:\t%x\n",UInt(i),refiller.io.wb.bits.keys(i))
      }
    }

    printf("=====Status in DRKStack( end )=====\n\n\n")
  }  


}

class WB_IO(implicit val p: Parameters) extends ParameterizedBundle()(p) 
with HasDRKStackParameters
with HasDRParameters {
  val stack_cached_base = UInt(width=paddrBits)
  val keys = Vec(drkStackShiftAmt,UInt(OUTPUT,width=drKeyBits))
}
class FETCH_RESP_IO(implicit val p: Parameters) extends ParameterizedBundle()(p)
with HasDRKStackParameters
with HasDRParameters {
  val keys = Vec(drkStackShiftAmt,UInt(OUTPUT,width=drKeyBits))
}


class DRKStackRefiller(implicit p:Parameters) extends DRModule()(p)
with HasDRKStackParameters {

  val io = new Bundle {
    val wb = Decoupled( new WB_IO()(p)).flip
    val fetch = Decoupled(
                new Bundle{
                  val stack_cached_base = UInt(OUTPUT)
                }
            ).flip
    val fetch_resp = new FETCH_RESP_IO()(p)


    val mem = new ClientUncachedTileLinkIO

  }

  //try to hold a line always
 
  val s_ready :: s_wb_valid :: s_wb_wait :: s_fetch_valid :: s_fetch_wait :: s_fetch_shift :: Nil = Enum(UInt(),6)
  val state = Reg(init=s_ready)
  val top_buffer = Reg(init=Vec.fill(drkStackShiftAmt){UInt(0,width=drKeyBits)})
  val bot_buffer = Reg(init=Vec.fill(drkStackShiftAmt){UInt(0,width=drKeyBits)})
  val top_valid = Reg(init=Bool(false))
  val bot_valid = Reg(init=Bool(false))
  val top_addr_block = Reg(UInt())
  val bot_addr_block = Reg(UInt())

  io.wb.ready := !top_valid || !bot_valid
  io.fetch.ready := top_valid 
  io.fetch_resp.keys := top_buffer

  val const_client_xact_id = UInt(0)
  val reg_cnt_beats = Reg(init=UInt(0,width=3))


  when(state === s_ready){
    //!bot_valid always
    //!top_valid && !bot_valid for init, top_valid && !bot_valid otherwise
    reg_cnt_beats := UInt(0)
    when(io.wb.fire()){
      for(i <- 0 until drkStackShiftAmt){
        top_buffer(UInt(i)) := io.wb.bits.keys(UInt(i))
      }       
      top_addr_block := io.wb.bits.stack_cached_base(31,6)
      top_valid := Bool(true)
      when(!top_valid && !bot_valid){
	state := s_ready
      }.elsewhen(top_valid && !bot_valid){
        bot_buffer := top_buffer
	bot_addr_block := top_addr_block
	bot_valid := top_valid
	state := s_wb_valid
      }
    }.elsewhen(io.fetch.fire()){
      when(top_valid){
        top_valid := Bool(false)
        bot_addr_block := (io.fetch.bits.stack_cached_base(31,6) - UInt(2)) //prepare
      }
      state := s_fetch_valid
    }
  }
  when(state === s_wb_valid){
  //top_valid && bot_valid initially.
  //bot_valid always
    when(io.fetch.fire()){
      top_valid := Bool(false)
    }.elsewhen(io.wb.fire()){
      for(i <- 0 until drkStackShiftAmt){
        top_buffer(UInt(i)) := io.wb.bits.keys(UInt(i))
      }       
      top_addr_block := io.wb.bits.stack_cached_base(31,6)
      top_valid := Bool(true)
    }
    when(io.mem.acquire.fire()){
      when(reg_cnt_beats === UInt(3)){
        state := s_wb_wait
      }
      reg_cnt_beats := reg_cnt_beats + UInt(1)
    }
  }

  when(state === s_wb_wait){
  //bot_valid always
    when (io.mem.grant.fire() & io.mem.grant.bits.client_xact_id === const_client_xact_id){
      when(!top_valid){
        when(io.wb.fire()){
          for(i <- 0 until drkStackShiftAmt){
            top_buffer(UInt(i)) := io.wb.bits.keys(UInt(i))
          }         
          top_addr_block := io.wb.bits.stack_cached_base(31,6)
        }.otherwise{
	  top_buffer := bot_buffer
	  top_addr_block := bot_addr_block
	  bot_valid := Bool(false)
        }
      }.otherwise{
        bot_valid := Bool(false)
        when(io.fetch.fire()){
	  top_buffer := bot_buffer
	  top_addr_block := bot_addr_block
	}	  
      }
      bot_valid := Bool(false)
      top_valid := Bool(true)
      state := s_ready
    }.elsewhen(io.fetch.fire()){
      top_valid := Bool(false)
    }.elsewhen(io.wb.fire()){
      for(i <- 0 until drkStackShiftAmt){
        top_buffer(UInt(i)) := io.wb.bits.keys(UInt(i))
      }       
      top_addr_block := io.wb.bits.stack_cached_base(31,6)
      top_valid := Bool(true)
    }
  }
  when(state === s_fetch_valid){ 
    when(io.fetch.fire()){
      top_valid := Bool(false)
    }.elsewhen(io.wb.fire()){
      for(i <- 0 until drkStackShiftAmt){
        top_buffer(UInt(i)) := io.wb.bits.keys(UInt(i))
      }       
      top_addr_block := io.wb.bits.stack_cached_base(31,6)
      top_valid := Bool(true)
    }
    when(io.mem.acquire.fire()){
      state := s_fetch_wait
    }
  }
  when(state === s_fetch_wait){ 
    when (io.mem.grant.fire() & io.mem.grant.bits.client_xact_id === const_client_xact_id){
      when(top_valid){
        when(io.fetch.fire()){
	  top_valid := Bool(false)
	}
      }.otherwise{
        when(io.wb.fire()){
          for(i <- 0 until drkStackShiftAmt){
            top_buffer(UInt(i)) := io.wb.bits.keys(UInt(i))
          }       
          top_addr_block := io.wb.bits.stack_cached_base(31,6)
          top_valid := Bool(true)
        }
      }
      bot_buffer(Cat(reg_cnt_beats,UInt(1))) := io.mem.grant.bits.data(127,64)
      bot_buffer(Cat(reg_cnt_beats,UInt(0))) := io.mem.grant.bits.data(63,0)
      reg_cnt_beats := reg_cnt_beats + UInt(1)
      when(reg_cnt_beats === UInt(3)){
        state := s_fetch_shift
      }
    }.elsewhen(io.fetch.fire()){
      top_valid := Bool(false)
    }.elsewhen(io.wb.fire()){
      for(i <- 0 until drkStackShiftAmt){
        top_buffer(UInt(i)) := io.wb.bits.keys(UInt(i))
      }       
      top_addr_block := io.wb.bits.stack_cached_base(31,6)
      top_valid := Bool(true)
    }
  }
  when(state === s_fetch_shift){
    when(top_valid){
      when(io.fetch.fire()){
        top_buffer := bot_buffer
	top_addr_block := bot_addr_block
      }.otherwise{
        bot_valid := Bool(false)
      }
    }.otherwise{
      when(io.wb.fire()){
        for(i <- 0 until drkStackShiftAmt){
          top_buffer(UInt(i)) := io.wb.bits.keys(UInt(i))
        }       
        top_addr_block := io.wb.bits.stack_cached_base(31,6)
      }.otherwise{

	top_buffer := bot_buffer
	top_addr_block := bot_addr_block
      }
    }
    bot_valid := Bool(false)
    top_valid := Bool(true)
    state := s_ready
  }


  //mem signals
  val acquire_data = Cat(bot_buffer(Cat(reg_cnt_beats,UInt(1))),bot_buffer(Cat(reg_cnt_beats,UInt(0))))
  val acquire_wb = PutBlock(client_xact_id = const_client_xact_id,
                            addr_block = bot_addr_block,
                            addr_beat = reg_cnt_beats,
                            data = acquire_data)(p)
  val acquire_fetch = GetBlock(client_xact_id = const_client_xact_id,
                               addr_block = bot_addr_block,
                               alloc = Bool(true))(p)

  io.mem.acquire.valid := state === s_wb_valid || state === s_fetch_valid
  io.mem.acquire.bits := Mux(state === s_wb_valid, acquire_wb, acquire_fetch)

  io.mem.grant.ready := state === s_wb_wait || state === s_fetch_wait

  if(debugDRKStack && debugRefiller ){
    printf("=====Status in DRKStackRefiller(begin)=====\n\n\n")
    printf("\ttop_valid:\t%x\n",top_valid)
    printf("\tbot_valid:\t%x\n",bot_valid)
    printf("\tstate:\t%x\n",state)
    for(i <- 0 until drkStackShiftAmt){
      printf("\t\ttop_buffer(%d):\t%x\n",UInt(i),top_buffer(i))
    }
    for(i <- 0 until drkStackShiftAmt){
      printf("\t\tbot_buffer(%d):\t%x\n",UInt(i),bot_buffer(i))
    }
    printf("\ttop_base_addr:\t%x\n",Cat(top_addr_block,UInt(0,width=6)))
    printf("\tbot_base_addr:\t%x\n",Cat(bot_addr_block,UInt(0,width=6)))
    when(top_valid && bot_valid){
      printf("\ttop-bot:\t%x\n",top_addr_block-bot_addr_block)
    }
   when(io.fetch.fire()){
     printf("\tfetch fire in Refiller\n")
     printf("\ttop_base_addr:\t%x\n",Cat(top_addr_block,UInt(0,width=6)))
     printf("\tio.fetch.bits.stack_cached_base:\t%x\n",io.fetch.bits.stack_cached_base)
   }

    when(io.mem.acquire.fire())
    {
      when(state === s_wb_valid){
        printf("\twb acquire req fires\n")
        printf("\t\tbase_addr:\t%x\n",Cat(io.mem.acquire.bits.addr_block,UInt(0,width=6)))
        printf("\t\taddr_beat:\t%x\n",io.mem.acquire.bits.addr_beat)
        printf("\t\tdata:\t%x\n",io.mem.acquire.bits.data)
      }
      when(state === s_fetch_valid){
        printf("\tfetch acquire req fires\n")
        printf("\t\tbase_addr:\t%x\n",Cat(io.mem.acquire.bits.addr_block,UInt(0,width=6)))
      }
    }
    when(io.mem.grant.fire()){
      printf("\tgrant fires\n")
      printf("\t\tclient_xact_id:\t%x\n",io.mem.grant.bits.client_xact_id)
    }
    printf("=====Status in DRKStackRefiller( end )=====\n\n\n")
  }
  
}

class DRKStackWrapper(implicit p: Parameters) extends DRModule()(p)
with HasDRKStackParameters{

  val io = new Bundle{

    val pipe_in = Decoupled(new DRKStackWrapperReq()(p)).flip
    val pipe_out = new DRKStackWrapperResp()(p)

  }

  val inner_stack = SeqMem(drkStackSize/drkStackShiftAmt,Vec.fill(drkStackShiftAmt){UInt(width=drKeyBits)})

  io.pipe_in.ready := Bool(true)
  val s0_id = io.pipe_in.bits.id
  val s0_write = io.pipe_in.bits.write
  val s0_addr = io.pipe_in.bits.addr
  val s0_line_addr = addrToAddr(s0_addr)
  val s0_offset = addrToOffset(s0_addr)
  val s0_wdata = io.pipe_in.bits.wdata
  val s0_wdata_wide = io.pipe_in.bits.wdata_wide
  val s0_valid = io.pipe_in.valid
  val s0_wide = io.pipe_in.bits.wide
  val s0_mask = Mux(s0_wide, UInt(0xFF), addrToMask(s0_addr))
  val s0_wdata_vec = Wire(Vec.fill(drkStackShiftAmt){UInt()})
  for (i <- 0 until drkStackShiftAmt){
    s0_wdata_vec(i) := Mux(s0_wide, s0_wdata_wide(i),s0_wdata)
  }
  val s0_mask_vec = Wire(Vec.fill(drkStackShiftAmt){Bool()})
  for(i <- 0 until drkStackShiftAmt){
    s0_mask_vec(i) := s0_mask(i)
  }
  val s1_data_wide = Wire(init=Vec.fill(drkStackShiftAmt){UInt(width=drKeyBits)})
  val r_line_addr = Mux(s0_write, s0_line_addr + UInt(1), s0_line_addr)
  val w_line_addr = s0_line_addr

  when(s0_valid && s0_write){
    inner_stack.write(w_line_addr,s0_wdata_vec,s0_mask_vec)
  }
  val s0_r_valid = Wire(init=Bool(false))
  s0_r_valid := s0_valid && (!s0_write)
  s1_data_wide := inner_stack.read(r_line_addr, s0_r_valid)

  val s1_valid = Reg(init=Bool(false))
  s1_valid := s0_valid
  val s1_offset = Reg(init=UInt(0))
  s1_offset := s0_offset
  val s1_data = s1_data_wide(s1_offset)
  val s1_id = Reg(init=UInt(0))
  s1_id := s0_id
  val s1_line_addr = Reg(UInt(0))
  s1_line_addr := s0_line_addr
  io.pipe_out.id := s1_id
  io.pipe_out.rdata := s1_data
  io.pipe_out.rdata_wide := s1_data_wide
  io.pipe_out.valid := s1_valid
  val s1_wide = Reg(init=Bool(false))
  s1_wide := s0_wide

  if(debugDRKStack && debugStackWrapper){
    printf("\n\n=====Status in DRKStackWrapper(begin)=====\n")

    printf("\ts0_wide:\t%x\n",s0_wide)
    when(s0_wide && s0_write){
      for(i <- 0 until 8){
        printf("s0_wdata_wide(%d):\t%x\n",UInt(i),s0_wdata_wide(UInt(i)))
      }
    }
    printf("\ts0_write:\t%x\n",s0_write)
    printf("\ts0_valid:\t%x\n",s0_valid)
    printf("\ts0_addr:\t%x\n",s0_addr)
    printf("\ts0_id:\t%x\n",s0_id) 
    printf("\ts1_valid:\t%x\n",s1_valid)
    printf("\ts1_rdata:\t%x\n",s1_data)
    printf("\ts1_id:\t%x\n",s1_id)
       
    val s1_write = Reg(next=s0_write)
    when(s1_write && s1_valid){
      for(i <-0 until drkStackSize/drkStackShiftAmt){
        for(j <- 0 until drkStackShiftAmt){
          when(s1_wide){
	    when(s1_line_addr === UInt(i)){
	      when(UInt(j) === UInt(0)){
	        printf("========Written to this line========\n")
	      }
	      printf("========This Offset===========\n")
	      }
	  }.otherwise{
	    when(s1_line_addr === UInt(i) && s1_offset === UInt(j)){
	      printf("=========Written here==========\n")
	    }
	  }
	  when(Cat(UInt(i),UInt(j)) =/= s0_addr){
	    printf("\tinner_stack[%d][%d](%x):\t%x\n",UInt(i),UInt(j),
	    		Cat(UInt(i),UInt(j)),inner_stack.read(UInt(i))(UInt(j)))
	  }.otherwise{
	    printf("\ts0 write to:\t[%d][%d](%x)\n",UInt(i),UInt(j),(Cat(UInt(i),UInt(j))))
	  }
	}
      }
    }
    printf("\n\n=====Status in DRKStackWrapper( end )=====\n")
  }

}





