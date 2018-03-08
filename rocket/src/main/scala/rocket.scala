// See LICENSE for license details.

package rocket

import Chisel._
import junctions._
import uncore._
import Util._
import cde.{Parameters, Field}

case object UseFPU extends Field[Boolean]
case object FDivSqrt extends Field[Boolean]
case object XLen extends Field[Int]
case object FetchWidth extends Field[Int]
case object RetireWidth extends Field[Int]
case object UseVM extends Field[Boolean]
case object UsePerfCounters extends Field[Boolean]
case object FastLoadWord extends Field[Boolean]
case object FastLoadByte extends Field[Boolean]
case object FastMulDiv extends Field[Boolean]
case object CoreInstBits extends Field[Int]
case object CoreDataBits extends Field[Int]
case object CoreDCacheReqTagBits extends Field[Int]
case object NCustomMRWCSRs extends Field[Int]
case object MtvecInit extends Field[BigInt]

trait HasCoreParameters extends HasAddrMapParameters 
with HasMetaDRParameters
{
  implicit val p: Parameters
  val xLen = p(XLen)

  val usingVM = p(UseVM)
  val usingFPU = p(UseFPU)
  val usingFDivSqrt = p(FDivSqrt)
  val usingRoCC = !p(BuildRoCC).isEmpty
  val usingFastMulDiv = p(FastMulDiv)
  val fastLoadWord = p(FastLoadWord)
  val fastLoadByte = p(FastLoadByte)

  val retireWidth = p(RetireWidth)
  val fetchWidth = p(FetchWidth)
  val coreInstBits = p(CoreInstBits)
  val coreInstBytes = coreInstBits/8
  val coreDataBits = xLen
  val coreDataBytes = coreDataBits/8
  val coreDCacheReqTagBits = 7 + (2 + (if(!usingRoCC) 0 else 1))
  val coreMaxAddrBits = math.max(ppnBits,vpnBits+1) + pgIdxBits
  val vaddrBitsExtended = vaddrBits + (vaddrBits < xLen).toInt
  val mmioBase = p(MMIOBase)
  val nCustomMrwCsrs = p(NCustomMRWCSRs)
  val roccCsrs = if (p(BuildRoCC).isEmpty) Nil
    else p(BuildRoCC).flatMap(_.csrs)
  val nRoccCsrs = p(RoccNCSRs)
  val nCores = p(HtifKey).nCores
  val mtvecInit = p(MtvecInit)
  val startAddr = mtvecInit + 0x100

  // Print out log of committed instructions and their writeback values.
  // Requires post-processing due to out-of-order writebacks.
  val enableCommitLog = false
  val usingPerfCounters = p(UsePerfCounters)

  if (fastLoadByte) require(fastLoadWord)

  require(drKeyBits == 64)
  require(drEqClassIdBits == 11)


}

abstract class CoreModule(implicit val p: Parameters) extends Module
  with HasCoreParameters
abstract class CoreBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasCoreParameters

class RegFile(n: Int, w: Int, zero: Boolean = false) {
  private val rf = Mem(n, UInt(width = w))
  private def access(addr: UInt) = rf(~addr(log2Up(n)-1,0))
  private val reads = collection.mutable.ArrayBuffer[(UInt,UInt)]()
  private var canRead = true
  def read(addr: UInt) = {
    require(canRead)
    reads += addr -> Wire(UInt())
    reads.last._2 := Mux(Bool(zero) && addr === UInt(0), UInt(0), access(addr))
    reads.last._2
  }
  def write(addr: UInt, data: UInt) = {
    canRead = false
    when (addr =/= UInt(0)) {
      access(addr) := data
      for ((raddr, rdata) <- reads)
        when (addr === raddr) { rdata := data }
    }
  }
}

object ImmGen {
  def apply(sel: UInt, inst: UInt) = {
    val sign = Mux(sel === IMM_Z, SInt(0), inst(31).toSInt)
    val b30_20 = Mux(sel === IMM_U, inst(30,20).toSInt, sign)
    val b19_12 = Mux(sel =/= IMM_U && sel =/= IMM_UJ, sign, inst(19,12).toSInt)
    val b11 = Mux(sel === IMM_U || sel === IMM_Z, SInt(0),
              Mux(sel === IMM_UJ, inst(20).toSInt,
              Mux(sel === IMM_SB, inst(7).toSInt, sign)))
    val b10_5 = Mux(sel === IMM_U || sel === IMM_Z, Bits(0), inst(30,25))
    val b4_1 = Mux(sel === IMM_U, Bits(0),
               Mux(sel === IMM_S || sel === IMM_SB, inst(11,8),
               Mux(sel === IMM_Z, inst(19,16), inst(24,21))))
    val b0 = Mux(sel === IMM_S, inst(7),
             Mux(sel === IMM_I, inst(20),
             Mux(sel === IMM_Z, inst(15), Bits(0))))

    Cat(sign, b30_20, b19_12, b11, b10_5, b4_1, b0).toSInt
  }
}

class Rocket(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val host = new HtifIO
    val imem  = new FrontendIO()(p.alterPartial({case CacheName => "L1I" }))
    val dmem = new HellaCacheIO()(p.alterPartial({ case CacheName => "L1D" }))
    val ptw = new DatapathPTWIO().flip
    val fpu = new FPUIO().flip
    val rocc = new RoCCInterface().flip



    val kcache_fast_path_in = Valid(
      new Bundle{
        val id = UInt(OUTPUT)
	val key = UInt(OUTPUT)
      }
    ).flip
    val kcache_fast_path_out = Valid(
      new Bundle{
        val matched_at_id = Bool(OUTPUT)
        val id = UInt(OUTPUT)
      }
    )

    val static_table_base_out = UInt(OUTPUT)
    val static_table_base_in = UInt(INPUT)
    val dr_mask = Decoupled{
      new Bundle{
        val idmask = UInt(OUTPUT)
      }
    }.flip
    val status_stack = (new Bundle{
      val cannot_pop = Bool(OUTPUT)
      val cannot_push = Bool(OUTPUT)
      val handling_pop = Bool(OUTPUT)
      val handling_push = Bool(OUTPUT)
      val dry_xoring = Bool(OUTPUT)
      val need_wb = Bool(OUTPUT)

    }).flip
    val drkstack = Decoupled{new DRKStackTopCMDIO}
    val dr_config = new DRKConfigIO 
    val dr_status = (new DRKStatusIO).flip
    val dr_enable = Bool(INPUT)

    //key re returned the next cycle
    val drkstack_rf_req = Decoupled(new Bundle{
      val id = UInt(OUTPUT)
    })

    val drkstack_rf_resp = Decoupled( new Bundle{
        val key = UInt(OUTPUT)
      }
    ).flip

  }

  val dry_xoring = io.status_stack.dry_xoring

  var decode_table = XDecode.table
  if (usingFPU) decode_table ++= FDecode.table
  if (usingFPU && usingFDivSqrt) decode_table ++= FDivSqrtDecode.table
  if (usingRoCC) decode_table ++= RoCCDecode.table





  val ex_ctrl = Reg(new IntCtrlSigs)
  val mem_ctrl = Reg(new IntCtrlSigs)
  val wb_ctrl = Reg(new IntCtrlSigs)

  val ex_reg_xcpt_interrupt  = Reg(Bool())
  val ex_reg_valid           = Reg(Bool())
  val ex_reg_btb_hit         = Reg(Bool())
  val ex_reg_btb_resp        = Reg(io.imem.btb_resp.bits)
  val ex_reg_xcpt            = Reg(Bool())
  val ex_reg_flush_pipe      = Reg(Bool())
  val ex_reg_load_use        = Reg(Bool())
  val ex_reg_cause           = Reg(UInt())
  val ex_reg_pc = Reg(UInt())
  val ex_reg_inst = Reg(Bits())
  //data-rando
  val ex_reg_dr_fast_path_match = Reg(init=Bool(false))
  val ex_reg_dr_need_global_key    = Reg(init=Bool(false))
  val ex_reg_dr_need_dyn_key    = Reg(init=Bool(false))
  val ex_dr_need_key = ex_reg_dr_need_global_key || ex_reg_dr_need_dyn_key
  val ex_reg_dr_st_key	     = Reg(init=UInt(0,width=coreDataBits))
  val ex_reg_dr_is_mmgl         = Reg(init=Bool(false))
  val ex_reg_dr_do_xor		= Reg(init=Bool(false))
  val ex_reg_dr_id		= Reg(UInt())

  val mem_reg_xcpt_interrupt  = Reg(Bool())
  val mem_reg_valid           = Reg(Bool())
  val mem_reg_btb_hit         = Reg(Bool())
  val mem_reg_btb_resp        = Reg(io.imem.btb_resp.bits)
  val mem_reg_xcpt            = Reg(Bool())
  val mem_reg_replay          = Reg(Bool())
  val mem_reg_flush_pipe      = Reg(Bool())
  val mem_reg_cause           = Reg(UInt())
  val mem_reg_slow_bypass     = Reg(Bool())
  val mem_reg_pc = Reg(UInt())
  val mem_reg_inst = Reg(Bits())
  val mem_reg_wdata = Reg(Bits())
  val mem_reg_rs2 = Reg(Bits())
  val take_pc_mem = Wire(Bool())

  val mem_reg_dr_rand_data  = Reg(UInt())
  val mem_reg_dr_need_global_key    = Reg(init=Bool(false))
  val mem_reg_dr_need_dyn_key    = Reg(init=Bool(false))
  val mem_dr_need_key = mem_reg_dr_need_global_key || mem_reg_dr_need_dyn_key
  val mem_reg_dr_key         = Reg(UInt())
  val mem_reg_dr_is_mmgl  = Reg(init=Bool(false))
  val mem_reg_dr_do_xor		= Reg(init=Bool(false))
  val mem_reg_dr_id		= Reg(UInt())

  val wb_reg_valid           = Reg(Bool())
  val wb_reg_xcpt            = Reg(Bool())
  val wb_reg_replay          = Reg(Bool())
  val wb_reg_cause           = Reg(UInt())
  val wb_reg_rocc_pending    = Reg(init=Bool(false))
  val wb_reg_pc = Reg(UInt())
  val wb_reg_inst = Reg(Bits())
  val wb_reg_wdata = Reg(Bits())
  val wb_reg_rs2 = Reg(Bits())
  val take_pc_wb = Wire(Bool())
  val take_pc_mem_wb = take_pc_wb || take_pc_mem
  val take_pc = take_pc_mem_wb

  //for data-rando
  val wb_reg_dr_key         = Reg(init=UInt(0,width=64))
  val wb_reg_dr_need_global_key       = Reg(init=Bool(false))
  val wb_reg_dr_need_dyn_key       = Reg(init=Bool(false))
  val wb_dr_need_key = wb_reg_dr_need_global_key || wb_reg_dr_need_dyn_key
  val wb_reg_dr_is_mmgl         = Reg(init=Bool(false))
  val wb_reg_dr_do_xor		= Reg(init=Bool(false))
  val wb_reg_dr_id		= Reg(UInt())
  // decode stage
  val id_pc = io.imem.resp.bits.pc
  val id_inst = io.imem.resp.bits.data(0).toBits; require(fetchWidth == 1)
  val id_ctrl = Wire(new IntCtrlSigs()).decode(id_inst, decode_table)

  //data-rando



  val id_dr_is_mmgl = isMMGL(id_inst)
  val id_dr_is_mmll = isMMLL(id_inst)
  val id_dr_id_decoded = Mux(id_dr_is_mmgl || id_dr_is_mmll , getMMSrc(getMMIDs(id_inst)), getDRId(id_inst))

  val id_dr_do_xor = isStoreMask(id_inst) || isLoadUnmask(id_inst)


  val id_dr_global_q =  io.dr_mask
  val id_dr_class_id_from_global_q = id_dr_global_q.bits.idmask(drEqClassIdBits+coreDataBits-1,coreDataBits)
  val id_dr_global_q_valid = id_dr_global_q.valid
  val id_dr_key = id_dr_global_q.bits.idmask(coreDataBits-1,0)

  val id_dr_need_global_key = needsGlobalKey(id_inst)


  val id_dr_fast_path_match = io.kcache_fast_path_in.bits.id === id_dr_id_decoded &&
					io.kcache_fast_path_in.valid

  val id_dr_class_id_match = (id_dr_class_id_from_global_q === id_dr_id_decoded && id_dr_global_q_valid) ||
				(id_dr_fast_path_match && io.kcache_fast_path_in.valid)
 
  val ctrl_killd = Wire(Bool()) //brought up
  io.kcache_fast_path_out.bits.matched_at_id := id_dr_class_id_match && !ctrl_killd
  val id_dr_need_dyn_key = needsLocalKey(id_inst)


  def isPopAddr(inst: UInt): Bool = inst(31,20) === UInt(0xC5)
  def isPushAddr(inst: UInt): Bool = inst(31,20) === UInt(0xC4)
  val ctrl_stalld = Wire(init=Bool(false))
  val id_dr_no_ms_in_pipeline = (!ex_reg_valid  || (!isPopAddr(ex_reg_inst)  && !isPushAddr(ex_reg_inst))  || ex_ctrl.csr  === CSR.N) &&
  			        (!mem_reg_valid || (!isPopAddr(mem_reg_inst) && !isPushAddr(mem_reg_inst)) || mem_ctrl.csr === CSR.N) && 
				(!wb_reg_valid  || (!isPopAddr(wb_reg_inst)  && !isPushAddr(wb_reg_inst))  || wb_ctrl.csr  === CSR.N)
  

  val id_dr_is_push = Wire(init=Bool(false)) 
  val id_dr_is_pop = Wire(init=Bool(false))
  val wb_dr_push_passed = Wire(init=Bool(false))
  val wb_dr_pop_passed = Wire(init=Bool(false))


  val id_dr_is_stack_managing = (id_dr_is_push || id_dr_is_pop ) && !ctrl_killd
  val ms_ready :: ms_flowing :: ms_wait_1 :: ms_wait_2 ::  Nil = Enum(UInt(),4)
  val ms = Reg(init=ms_ready)
  when(ms === ms_ready){
    when(id_dr_is_stack_managing){
      ms := ms_flowing
    }
  }.elsewhen(ms === ms_flowing){
    when(wb_dr_push_passed || wb_dr_pop_passed){
      when(io.status_stack.handling_push || io.status_stack.handling_pop){
        ms := ms_wait_2
      }
      ms := ms_wait_1
    }.elsewhen(id_dr_no_ms_in_pipeline){ //gone
      ms := ms_wait_2
    }
  }.elsewhen(ms === ms_wait_1){
    when(io.status_stack.handling_push || io.status_stack.handling_pop){
      ms := ms_wait_2
    }
    //.elsewhen(id_dr_no_ms_in_pipeline){ //gone
    //  ms := ms_wait_2
    //}
  }.elsewhen(ms === ms_wait_2){
    when(!io.status_stack.cannot_pop && !io.status_stack.cannot_push){
      ms := ms_ready
    }
  }
  val id_dr_stall = Wire(init=Bool(false))
  val num_id_dr_stall = Reg(init=UInt(0,width=64))
  when(id_dr_stall){
    num_id_dr_stall := num_id_dr_stall + UInt(1)
  }.otherwise{
    num_id_dr_stall := UInt(0)
  }
  val id_dr_xcpt = num_id_dr_stall > UInt(10000)
  
  val id_dr_need_key = id_dr_need_dyn_key || id_dr_need_global_key                  
    
  io.kcache_fast_path_out.valid := io.imem.resp.valid && id_dr_need_global_key
  io.kcache_fast_path_out.bits.id := id_dr_id_decoded

  io.drkstack_rf_req.valid := id_dr_need_dyn_key
  io.drkstack_rf_req.bits.id := id_dr_id_decoded
  val o_stalld = Wire(Bool())
  io.dr_mask.ready :=  id_dr_need_global_key & !o_stalld & !id_dr_fast_path_match
  


  val id_raddr3 = id_inst(31,27)
  val id_raddr2 = id_inst(24,20)
  val id_raddr1 = id_inst(19,15)
  val id_waddr  = id_inst(11,7)
  val id_load_use = Wire(Bool())
  val id_reg_fence = Reg(init=Bool(false))
  val id_ren = IndexedSeq(id_ctrl.rxs1, id_ctrl.rxs2)
  val id_raddr = IndexedSeq(id_raddr1, id_raddr2)
  val rf = new RegFile(31, xLen)
  val id_rs = id_raddr.map(rf.read _)
  //val ctrl_killd = Wire(Bool()) // went up


  val csr = Module(new CSRFile)
  csr.io.static_table_base_in := io.static_table_base_in
  io.static_table_base_out := csr.io.static_table_base_out
  csr.io.drkstack <> io.drkstack
  io.dr_config := csr.io.dr_config
  csr.io.dr_status := io.dr_status
  val id_csr_en = id_ctrl.csr =/= CSR.N
  val id_system_insn = id_ctrl.csr === CSR.I
  val id_csr_ren = (id_ctrl.csr === CSR.S || id_ctrl.csr === CSR.C) && id_raddr1 === UInt(0)
  val id_csr_addr = id_inst(31,20)
  val id_csr = Mux(id_csr_ren && id_csr_addr =/= CSRs.dr_push && id_csr_addr =/= CSRs.dr_pop, CSR.R, id_ctrl.csr)
  // this is overly conservative
  val safe_csrs = CSRs.sscratch :: CSRs.sepc :: CSRs.mscratch :: CSRs.mepc :: CSRs.mcause :: CSRs.mbadaddr :: Nil
  val legal_csrs = collection.mutable.LinkedHashSet(CSRs.all:_*)
  val id_csr_flush = id_system_insn || (id_csr_en && !id_csr_ren && !DecodeLogic(id_csr_addr, safe_csrs.map(UInt(_)), (legal_csrs -- safe_csrs).toList.map(UInt(_))))
              
  val id_illegal_insn = !id_ctrl.legal ||
    id_ctrl.fp && !csr.io.status.fs.orR ||
    id_ctrl.rocc && !csr.io.status.xs.orR

  // stall decode for fences (now, for AMO.aq; later, for AMO.rl and FENCE)
  val id_amo_aq = id_inst(26)
  val id_amo_rl = id_inst(25)
  val id_fence_next = id_ctrl.fence || id_ctrl.amo && id_amo_rl
  val id_mem_busy = !io.dmem.ordered || io.dmem.req.valid
  val id_rocc_busy = Bool(usingRoCC) &&
    (io.rocc.busy || ex_reg_valid && ex_ctrl.rocc ||
     mem_reg_valid && mem_ctrl.rocc || wb_reg_valid && wb_ctrl.rocc)
  id_reg_fence := id_fence_next || id_reg_fence && id_mem_busy
  val id_do_fence = id_rocc_busy && id_ctrl.fence ||
    id_mem_busy && (id_ctrl.amo && id_amo_aq || id_ctrl.fence_i || id_reg_fence && (id_ctrl.mem || id_ctrl.rocc) || id_csr_en)

  val (id_xcpt, id_cause) = checkExceptions(List(
    (csr.io.interrupt,          csr.io.interrupt_cause),
    (io.imem.resp.bits.xcpt_if, UInt(Causes.fault_fetch)),
    (id_illegal_insn,           UInt(Causes.illegal_instruction)),
    (id_dr_xcpt && ms =/= ms_ready && ms === ms_flowing,		UInt(Causes.dr_1)),
    (id_dr_xcpt && ms =/= ms_ready && ms === ms_wait_1,		UInt(Causes.dr_2)),
    (id_dr_xcpt && ms =/= ms_ready && ms === ms_wait_2,		UInt(Causes.dr_3)),
    (id_dr_xcpt && ms === ms_ready,		UInt(Causes.id_xcpt_etc))
    ))
  id_dr_stall := (( io.imem.resp.valid ) && 
  			((id_dr_need_global_key && !id_dr_class_id_match) ||
			(ms =/= ms_ready && (id_dr_is_pop || id_dr_is_push)) ||
			(ms =/= ms_ready && io.status_stack.need_wb && id_dr_is_mmgl) ||
			(ms =/= ms_ready && ms =/= ms_wait_2 && id_dr_need_dyn_key) ||
		 	(id_dr_is_pop && io.status_stack.cannot_pop) ||
			(id_dr_is_push && io.status_stack.cannot_push) ||
			(id_dr_need_dyn_key && !io.drkstack_rf_req.ready) )) && 
			!id_xcpt


  val dmem_resp_xpu = !io.dmem.resp.bits.tag(0).toBool
  val dmem_resp_replay = io.dmem.resp.bits.replay && io.dmem.resp.bits.has_data
  val dr_l_bitmasks = Reg(init=Vec(32,UInt(0,width=64)))
  val ll_wen_pre = dmem_resp_replay && dmem_resp_xpu
  val ll_mask = Wire(init=UInt(0,width=coreDataBits))
  val dcache_bypass_data =
    if (fastLoadByte) Mux(needsMask(io.dmem.resp.bits.cmd), 
			io.dmem.resp.bits.data ^ (Mux(ll_wen_pre,ll_mask,wb_reg_dr_key)), 
			io.dmem.resp.bits.data)
    else if (fastLoadWord) Mux(needsMask(io.dmem.resp.bits.cmd), 
    			io.dmem.resp.bits.data_word_bypass ^ (Mux(ll_wen_pre,ll_mask,wb_reg_dr_key)), 
			io.dmem.resp.bits.data_word_bypass)
    else wb_reg_wdata 

  // detect bypass opportunities
  val ex_waddr = ex_reg_inst(11,7)
  val mem_waddr = mem_reg_inst(11,7)
  val wb_waddr = wb_reg_inst(11,7)
  val bypass_sources = IndexedSeq(
    (Bool(true), UInt(0), UInt(0)), // treat reading x0 as a bypass
    (ex_reg_valid && ex_ctrl.wxd, ex_waddr, mem_reg_wdata),
    (mem_reg_valid && mem_ctrl.wxd && !mem_ctrl.mem, mem_waddr, wb_reg_wdata),
    (mem_reg_valid && mem_ctrl.wxd, mem_waddr, dcache_bypass_data))
  val id_bypass_src = id_raddr.map(raddr => bypass_sources.map(s => s._1 && s._2 === raddr))

  // execute stage
  val bypass_mux = Vec(bypass_sources.map(_._3))
  val ex_reg_rs_bypass = Reg(Vec(id_raddr.size, Bool()))
  val ex_reg_rs_lsb = Reg(Vec(id_raddr.size, UInt()))
  val ex_reg_rs_msb = Reg(Vec(id_raddr.size, UInt()))
  val ex_rs = for (i <- 0 until id_raddr.size)
    yield Mux(ex_reg_rs_bypass(i), bypass_mux(ex_reg_rs_lsb(i)), Cat(ex_reg_rs_msb(i), ex_reg_rs_lsb(i)))
  val ex_imm = ImmGen(ex_ctrl.sel_imm, ex_reg_inst)
  val ex_op1 = MuxLookup(ex_ctrl.sel_alu1, SInt(0), Seq(
    A1_RS1 -> ex_rs(0).toSInt,
    A1_PC -> ex_reg_pc.toSInt))
  val ex_op2 = MuxLookup(ex_ctrl.sel_alu2, SInt(0), Seq(
    A2_RS2 -> ex_rs(1).toSInt,
    A2_IMM -> ex_imm,
    A2_FOUR -> SInt(4)))

  val alu = Module(new ALU)
  alu.io.dw := ex_ctrl.alu_dw
  alu.io.fn := ex_ctrl.alu_fn
  alu.io.in2 := ex_op2.toUInt
  alu.io.in1 := ex_op1.toUInt
  
  // multiplier and divider
  val div = Module(new MulDiv(width = xLen,
                              unroll = if(usingFastMulDiv) 8 else 1,
                              earlyOut = usingFastMulDiv))
  div.io.req.valid := ex_reg_valid && ex_ctrl.div
  div.io.req.bits.dw := ex_ctrl.alu_dw
  div.io.req.bits.fn := ex_ctrl.alu_fn
  div.io.req.bits.in1 := ex_rs(0)
  div.io.req.bits.in2 := ex_rs(1)
  div.io.req.bits.tag := ex_waddr

  ex_reg_valid := !ctrl_killd

  ex_reg_xcpt := !ctrl_killd && id_xcpt
  ex_reg_xcpt_interrupt := csr.io.interrupt && !take_pc && io.imem.resp.valid
  when (id_xcpt) { ex_reg_cause := id_cause }

  def ex_dr_dyn_key = io.drkstack_rf_resp.bits.key
  io.drkstack_rf_resp.ready := Bool(true)
  when (!ctrl_killd) {
    ex_reg_dr_do_xor := id_dr_do_xor
    ex_reg_dr_id := id_dr_id_decoded
    ex_reg_dr_need_global_key := id_dr_need_global_key 
    ex_reg_dr_need_dyn_key := id_dr_need_dyn_key
    ex_reg_dr_is_mmgl := id_dr_is_mmgl
    when(id_dr_need_global_key){
      ex_reg_dr_st_key := id_dr_key
    }.otherwise{
      ex_reg_dr_st_key := Cat(UInt(0xDEAD,width=16), UInt(0xBEEF,width=16), UInt(0xDEAD,width=16), UInt(0xBEEF,width=16))
    }
    ex_reg_dr_fast_path_match := id_dr_fast_path_match
    ex_ctrl := id_ctrl
    ex_ctrl.csr := id_csr
    ex_reg_btb_hit := io.imem.btb_resp.valid
    when (io.imem.btb_resp.valid) { ex_reg_btb_resp := io.imem.btb_resp.bits }
    ex_reg_flush_pipe := id_ctrl.fence_i || id_csr_flush
    ex_reg_load_use := id_load_use

    for (i <- 0 until id_raddr.size) {
      val do_bypass = id_bypass_src(i).reduce(_||_)
      val bypass_src = PriorityEncoder(id_bypass_src(i))
      ex_reg_rs_bypass(i) := do_bypass
      ex_reg_rs_lsb(i) := bypass_src
      when (id_ren(i) && !do_bypass) {
        ex_reg_rs_lsb(i) := id_rs(i)(bypass_src.getWidth-1,0)
        ex_reg_rs_msb(i) := id_rs(i) >> bypass_src.getWidth
      }
    }
  }
  when (!ctrl_killd || csr.io.interrupt) {
    ex_reg_inst := id_inst
    ex_reg_pc := id_pc
  }

  // replay inst in ex stage?
  val wb_dcache_miss = wb_ctrl.mem && !io.dmem.resp.valid
  val replay_ex_structural = ex_ctrl.mem && !io.dmem.req.ready ||
                             ex_ctrl.div && !div.io.req.ready
  val replay_ex_load_use = wb_dcache_miss && ex_reg_load_use
  val replay_ex = ex_reg_valid && (replay_ex_structural || replay_ex_load_use)
  val ctrl_killx = take_pc_mem_wb || replay_ex || !ex_reg_valid
  // detect 2-cycle load-use delay for LB/LH/SC
  val ex_slow_bypass = ex_ctrl.mem_cmd === M_XSC || Vec(MT_B, MT_BU, MT_H, MT_HU).contains(ex_ctrl.mem_type)

  val (ex_xcpt, ex_cause) = checkExceptions(List(
    (ex_reg_xcpt_interrupt || ex_reg_xcpt, ex_reg_cause),
    (ex_ctrl.fp && io.fpu.illegal_rm,      UInt(Causes.illegal_instruction))))



  // memory stage
  val mem_br_taken = mem_reg_wdata(0)
  val mem_br_target = mem_reg_pc.toSInt +
    Mux(mem_ctrl.branch && mem_br_taken, ImmGen(IMM_SB, mem_reg_inst),
    Mux(mem_ctrl.jal, ImmGen(IMM_UJ, mem_reg_inst), SInt(4)))
  val mem_int_wdata = Mux(mem_ctrl.jalr, mem_br_target, mem_reg_wdata.toSInt).toUInt
  val mem_npc = (Mux(mem_ctrl.jalr, Cat(vaSign(mem_reg_wdata, mem_reg_wdata), mem_reg_wdata(vaddrBits-1,0)).toSInt, mem_br_target) & SInt(-2)).toUInt
  val mem_wrong_npc = mem_npc =/= ex_reg_pc || !ex_reg_valid
  val mem_npc_misaligned = mem_npc(1)
  val mem_misprediction = mem_wrong_npc && mem_reg_valid && (mem_ctrl.branch || mem_ctrl.jalr || mem_ctrl.jal)
  val want_take_pc_mem = mem_reg_valid && (mem_misprediction || mem_reg_flush_pipe)
  take_pc_mem := want_take_pc_mem && !mem_npc_misaligned

  mem_reg_valid := !ctrl_killx
  mem_reg_replay := !take_pc_mem_wb && replay_ex
  mem_reg_xcpt := !ctrl_killx && ex_xcpt
  mem_reg_xcpt_interrupt := !take_pc_mem_wb && ex_reg_xcpt_interrupt
  when (ex_xcpt) { mem_reg_cause := ex_cause }

    val is_unsigned = Vec(MT_BU, MT_HU, MT_WU).contains(ex_ctrl.mem_type)
    val key_mask = Mux(ex_ctrl.mem && ex_ctrl.mem_type === MT_BU, UInt(0xFF,width=64),
    			Mux(ex_ctrl.mem && ex_ctrl.mem_type === MT_HU, Cat(UInt(0,width=48),UInt(0xFFFF,width=16)),
			Mux(ex_ctrl.mem && ex_ctrl.mem_type === MT_WU, Cat(UInt(0,width=32),UInt(0xFFFF,width=16),UInt(0xFFFF,width=16)),
			Cat(UInt(0xFFFF,width=16), UInt(0xFFFF,width=16), UInt(0xFFFF,width=16), UInt(0xFFFF,width=16)))))
 
  def signed_ashift(s: UInt, amt: UInt): UInt = s >> amt


  when (ex_reg_valid || ex_reg_xcpt_interrupt) {
    mem_ctrl := ex_ctrl
    mem_reg_btb_hit := ex_reg_btb_hit
    when (ex_reg_btb_hit) { mem_reg_btb_resp := ex_reg_btb_resp }
    mem_reg_flush_pipe := ex_reg_flush_pipe
    mem_reg_slow_bypass := ex_slow_bypass

    mem_reg_inst := ex_reg_inst
    mem_reg_pc := ex_reg_pc
    mem_reg_wdata := alu.io.out
    //data-rando
    mem_reg_dr_need_global_key := ex_reg_dr_need_global_key
    mem_reg_dr_need_dyn_key := ex_reg_dr_need_dyn_key
    mem_reg_dr_do_xor := ex_reg_dr_do_xor
    mem_reg_dr_id := ex_reg_dr_id

    when(ex_reg_dr_need_dyn_key){
      mem_reg_dr_key := key_mask & ( signed_ashift(ex_dr_dyn_key, Mux(!ex_ctrl.mem, UInt(0), (Cat(alu.io.adder_out(2,0), UInt(0,width=3))))))

    }.otherwise{
      when(ex_reg_dr_fast_path_match){
        mem_reg_dr_key := key_mask & (signed_ashift(io.kcache_fast_path_in.bits.key,Mux(!ex_ctrl.mem, UInt(0), (Cat(alu.io.adder_out(2,0), UInt(0,width=3))))))

      }.otherwise{
        mem_reg_dr_key := key_mask & (signed_ashift(ex_reg_dr_st_key, Mux(!ex_ctrl.mem, UInt(0), (Cat(alu.io.adder_out(2,0), UInt(0,width=3))))))
      }
    }
    mem_reg_dr_is_mmgl := ex_reg_dr_is_mmgl
    when (ex_ctrl.rxs2 && (ex_ctrl.mem || ex_ctrl.rocc)) {
      mem_reg_rs2 := ex_rs(1)
      //data-rando
        when(ex_reg_dr_need_global_key){
          mem_reg_dr_rand_data := ex_rs(1) ^ 
	    (signed_ashift(Mux(ex_reg_dr_fast_path_match,io.kcache_fast_path_in.bits.key,ex_reg_dr_st_key),
	    (Cat(alu.io.adder_out(2,0), UInt(0,width=3))))) 

        }.otherwise{
          mem_reg_dr_rand_data := ex_rs(1) ^ (
	    signed_ashift(ex_dr_dyn_key, (Cat(alu.io.adder_out(2,0), UInt(0,width=3))))) 
	}
    }
  }

  val (mem_xcpt, mem_cause) = checkExceptions(List(
    (mem_reg_xcpt_interrupt || mem_reg_xcpt,              mem_reg_cause),
    (want_take_pc_mem && mem_npc_misaligned,              UInt(Causes.misaligned_fetch)),
    (mem_reg_valid && mem_ctrl.mem && io.dmem.xcpt.ma.st, UInt(Causes.misaligned_store)),
    (mem_reg_valid && mem_ctrl.mem && io.dmem.xcpt.ma.ld, UInt(Causes.misaligned_load)),
    (mem_reg_valid && mem_ctrl.mem && io.dmem.xcpt.pf.st, UInt(Causes.fault_store)),
    (mem_reg_valid && mem_ctrl.mem && io.dmem.xcpt.pf.ld, UInt(Causes.fault_load))))

  val dcache_kill_mem = mem_reg_valid && mem_ctrl.wxd && io.dmem.replay_next.valid // structural hazard on writeback port
  val fpu_kill_mem = mem_reg_valid && mem_ctrl.fp && io.fpu.nack_mem
  val replay_mem  = dcache_kill_mem || mem_reg_replay || fpu_kill_mem
  val killm_common = dcache_kill_mem || take_pc_wb || mem_reg_xcpt || !mem_reg_valid
  div.io.kill := killm_common && Reg(next = div.io.req.fire())
  val ctrl_killm = killm_common || mem_xcpt || fpu_kill_mem

  // writeback stage
  wb_reg_valid := !ctrl_killm
  wb_reg_replay := replay_mem && !take_pc_wb 
  wb_reg_xcpt := mem_xcpt && !take_pc_wb
  when (mem_xcpt) { wb_reg_cause := mem_cause }

  val msb_addr = Mux(mem_ctrl.mem_type === MT_B, UInt(7),
  			Mux(mem_ctrl.mem_type === MT_H, UInt(15),
			Mux(mem_ctrl.mem_type === MT_W, UInt(31),
			UInt(63))))

  when (mem_reg_valid || mem_reg_replay || mem_reg_xcpt_interrupt) {
    wb_ctrl := mem_ctrl
    wb_reg_wdata := Mux(mem_ctrl.fp && mem_ctrl.wxd, io.fpu.toint_data, mem_int_wdata)
    when (mem_ctrl.rocc) {
      wb_reg_rs2 := mem_reg_rs2
    }
    for(i <- 0 until 64){
      when(UInt(i) > msb_addr){ 
      wb_reg_dr_key(i) := Mux(!mem_ctrl.mem, mem_reg_dr_key(i), 
    		          mem_reg_dr_key(msb_addr) 
			)
      }.otherwise{
      wb_reg_dr_key(i) := mem_reg_dr_key(i) 
      }
    }
    wb_reg_dr_do_xor := mem_reg_dr_do_xor
    wb_reg_dr_id := mem_reg_dr_id
    wb_reg_dr_is_mmgl := mem_reg_dr_is_mmgl
    wb_reg_dr_need_global_key := mem_reg_dr_need_global_key
    wb_reg_dr_need_dyn_key := mem_reg_dr_need_dyn_key
    wb_reg_inst := mem_reg_inst
    wb_reg_pc := mem_reg_pc

    

  }

  val wb_set_sboard = wb_ctrl.div || wb_dcache_miss || wb_ctrl.rocc
  val replay_wb_common = io.dmem.resp.bits.nack || wb_reg_replay
  val wb_rocc_val = wb_reg_valid && wb_ctrl.rocc && !replay_wb_common
  val replay_wb = replay_wb_common || wb_reg_valid && wb_ctrl.rocc && !io.rocc.cmd.ready  
  val wb_xcpt = wb_reg_xcpt || csr.io.csr_xcpt
  take_pc_wb := replay_wb || wb_xcpt || csr.io.eret

  when (wb_rocc_val) { wb_reg_rocc_pending := !io.rocc.cmd.ready }
  when (wb_reg_xcpt) { wb_reg_rocc_pending := Bool(false) }

  // writeback arbitration
  //val dmem_resp_xpu = !io.dmem.resp.bits.tag(0).toBool
  val dmem_resp_fpu =  io.dmem.resp.bits.tag(0).toBool
  val dmem_resp_waddr = io.dmem.resp.bits.tag.toUInt()(5,1)
  val dmem_resp_valid = io.dmem.resp.valid && io.dmem.resp.bits.has_data
  //val dmem_resp_replay = io.dmem.resp.bits.replay && io.dmem.resp.bits.has_data

  div.io.resp.ready := !(wb_reg_valid && wb_ctrl.wxd)
  val ll_wdata = Wire(init = div.io.resp.bits.data)
  val ll_waddr = Wire(init = div.io.resp.bits.tag)
  val ll_wen = Wire(init = div.io.resp.fire())
  if (usingRoCC) {
    io.rocc.resp.ready := !(wb_reg_valid && wb_ctrl.wxd)
    when (io.rocc.resp.fire()) {
      div.io.resp.ready := Bool(false)
      ll_wdata := io.rocc.resp.bits.data
      ll_waddr := io.rocc.resp.bits.rd
      ll_wen := Bool(true)
    }
  }
  
  when (dmem_resp_replay && dmem_resp_xpu) {
    div.io.resp.ready := Bool(false)
    if (usingRoCC)
      io.rocc.resp.ready := Bool(false)
    ll_waddr := dmem_resp_waddr
    ll_wen := Bool(true)
  }

  val wb_valid = wb_reg_valid && !replay_wb && !csr.io.csr_xcpt
  val wb_wen = wb_valid && wb_ctrl.wxd
  val rf_wen = wb_wen || ll_wen
  val rf_waddr = Mux(ll_wen, ll_waddr, wb_waddr)


  //decalred above
  //val ll_mask = Wire(init=UInt(0,width=coreDataBits))
  val wb_mask = Wire(init=UInt(0,width=coreDataBits))
  ll_mask := dr_l_bitmasks(ll_waddr)
  //wb_mask := Mux(dmem_resp_replay,ll_mask,Mux(wb_reg_dr_is_mmgl, UInt(0,width=coreDataBits),wb_reg_dr_key))
  wb_mask := Mux(dmem_resp_replay,ll_mask,Mux(wb_reg_dr_do_xor, wb_reg_dr_key, UInt(0,width=coreDataBits)))

  val ll_wdata_unmasked = ll_wdata 


  
  val wb_wdata_unmasked =  io.dmem.resp.bits.data ^ wb_mask
  val rf_wdata = Mux(dmem_resp_valid && dmem_resp_xpu, wb_wdata_unmasked,
                 Mux(ll_wen, ll_wdata_unmasked,
                 Mux(wb_ctrl.csr =/= CSR.N, csr.io.rw.rdata,
                 wb_reg_wdata)))

  when (rf_wen) { rf.write(rf_waddr, rf_wdata) }



  // hook up control/status regfile
  csr.io.exception := wb_reg_xcpt
  csr.io.cause := wb_reg_cause
  csr.io.retire := wb_valid
  io.host <> csr.io.host
  io.fpu.fcsr_rm := csr.io.fcsr_rm
  csr.io.fcsr_flags := io.fpu.fcsr_flags
  csr.io.rocc <> io.rocc
  csr.io.pc := wb_reg_pc
  csr.io.uarch_counters.foreach(_ := Bool(false))
  io.ptw.ptbr := csr.io.ptbr
  io.ptw.invalidate := csr.io.fatc
  io.ptw.status := csr.io.status
  csr.io.rw.addr := Mux(isMMGL(wb_reg_inst),CSRs.dr_mmgl,
                        Mux(isMMLL(wb_reg_inst),CSRs.dr_mmll,
                        wb_reg_inst(31,20)))
  csr.io.rw.cmd := Mux(!wb_reg_valid, CSR.N, 
  			Mux(isMM(wb_reg_inst) ,CSR.W,wb_ctrl.csr))

  csr.io.rw.wdata := Mux(isMM(wb_reg_inst),getMMIDs(wb_reg_inst),wb_reg_wdata)
  csr.io.rw.dr_key := wb_reg_dr_key
  val hazard_targets = Seq((id_ctrl.rxs1 && id_raddr1 =/= UInt(0), id_raddr1),
                           (id_ctrl.rxs2 && id_raddr2 =/= UInt(0), id_raddr2),
                           (id_ctrl.wxd  && id_waddr  =/= UInt(0), id_waddr))
  val fp_hazard_targets = Seq((io.fpu.dec.ren1, id_raddr1),
                              (io.fpu.dec.ren2, id_raddr2),
                              (io.fpu.dec.ren3, id_raddr3),
                              (io.fpu.dec.wen, id_waddr))
 
  id_dr_is_push := id_ctrl.csr =/= CSR.N && id_csr_addr === UInt(0xC4)
  id_dr_is_pop := id_ctrl.csr =/= CSR.N && id_csr_addr === UInt(0xC5)
     

  wb_dr_push_passed := csr.io.rw.cmd =/= CSR.N && csr.io.rw.addr === UInt(0xC4)
  wb_dr_pop_passed := csr.io.rw.cmd =/= CSR.N && csr.io.rw.addr === UInt(0xC5)

  //when(wb_set_sboard && wb_wen){
  //  dr_l_bitmasks(wb_waddr) := Mux(wb_reg_dr_is_mmgl, UInt(0, coreDataBits), wb_reg_dr_key)
 // }

  for(i <- 0 until 32){
    dr_l_bitmasks(UInt(i)) := Mux(wb_waddr === UInt(i) && wb_dcache_miss && wb_wen, Mux(wb_reg_dr_do_xor, wb_reg_dr_key, UInt(0)),  //set
    				Mux(ll_waddr === UInt(i) && ll_wen, UInt(0),//clear
				dr_l_bitmasks(UInt(i)))) //hold
  }
  val sboard = new Scoreboard(32)
  sboard.clear(ll_wen, ll_waddr)

  //hgmoon note: what happens if ll_wen === wb_sen === 1?
  //without rocc, this doesn't happen
  //with rocc?

  when(ll_wen){
    printf("ll_wen = 1\n")
    printf("ll_waddr:\t%x\n",ll_waddr)
    when(wb_wen){
      printf("ll_wen === wb_wen === 1\n")
    }
  }
  when(wb_set_sboard && wb_wen){
    printf("wb_set_sboard && wb_wen = 1\n")
    printf("wb_waddr:\t%x\n",wb_waddr)
    printf("addr,mask:\t%x,%x\n",wb_waddr,wb_reg_dr_key)
  }

  val id_sboard_hazard = checkHazards(hazard_targets, sboard.readBypassed _)
  sboard.set(wb_set_sboard && wb_wen, wb_waddr)

  // stall for RAW/WAW hazards on CSRs, loads, AMOs, and mul/div in execute stage.
  val ex_cannot_bypass = ex_ctrl.csr =/= CSR.N || ex_ctrl.jalr || ex_ctrl.mem || ex_ctrl.div || ex_ctrl.fp || ex_ctrl.rocc
  val data_hazard_ex = ex_ctrl.wxd && checkHazards(hazard_targets, _ === ex_waddr)
  val fp_data_hazard_ex = ex_ctrl.wfd && checkHazards(fp_hazard_targets, _ === ex_waddr)
  val id_ex_hazard = ex_reg_valid && (data_hazard_ex && ex_cannot_bypass || fp_data_hazard_ex)

  // stall for RAW/WAW hazards on CSRs, LB/LH, and mul/div in memory stage.
  val mem_mem_cmd_bh =
    if (fastLoadWord) Bool(!fastLoadByte) && mem_reg_slow_bypass
    else Bool(true)
  val mem_cannot_bypass = mem_ctrl.csr =/= CSR.N || mem_ctrl.mem && mem_mem_cmd_bh || mem_ctrl.div || mem_ctrl.fp || mem_ctrl.rocc
  val data_hazard_mem = mem_ctrl.wxd && checkHazards(hazard_targets, _ === mem_waddr)
  val fp_data_hazard_mem = mem_ctrl.wfd && checkHazards(fp_hazard_targets, _ === mem_waddr)
  val id_mem_hazard = mem_reg_valid && (data_hazard_mem && mem_cannot_bypass || fp_data_hazard_mem)
  id_load_use := mem_reg_valid && data_hazard_mem && mem_ctrl.mem

  // stall for RAW/WAW hazards on load/AMO misses and mul/div in writeback.
  val data_hazard_wb = wb_ctrl.wxd && checkHazards(hazard_targets, _ === wb_waddr)
  val fp_data_hazard_wb = wb_ctrl.wfd && checkHazards(fp_hazard_targets, _ === wb_waddr)
  val id_wb_hazard = wb_reg_valid && (data_hazard_wb && wb_set_sboard || fp_data_hazard_wb)

  val id_stall_fpu = if (usingFPU) {
    val fp_sboard = new Scoreboard(32)
    fp_sboard.set((wb_dcache_miss && wb_ctrl.wfd || io.fpu.sboard_set) && wb_valid, wb_waddr)
    fp_sboard.clear(dmem_resp_replay && dmem_resp_fpu, dmem_resp_waddr)
    fp_sboard.clear(io.fpu.sboard_clr, io.fpu.sboard_clra)

    id_csr_en && !io.fpu.fcsr_rdy || checkHazards(fp_hazard_targets, fp_sboard.read _)
  } else Bool(false)




  o_stalld :=
    id_ex_hazard || id_mem_hazard || id_wb_hazard || id_sboard_hazard ||
    id_ctrl.fp && id_stall_fpu ||
    id_ctrl.mem && !io.dmem.req.ready ||
    Bool(usingRoCC) && wb_reg_rocc_pending && id_ctrl.rocc && !io.rocc.cmd.ready ||
    id_do_fence ||
    csr.io.csr_stall
  
  ctrl_stalld := o_stalld || id_dr_stall


  ctrl_killd := !io.imem.resp.valid || take_pc || ctrl_stalld || csr.io.interrupt 

  io.imem.req.valid := take_pc
  io.imem.req.bits.pc :=
    Mux(wb_xcpt || csr.io.eret, csr.io.evec,     // exception or [m|s]ret
    Mux(replay_wb,              wb_reg_pc,       // replay
                                mem_npc)).toUInt // mispredicted branch
  io.imem.invalidate := wb_reg_valid && wb_ctrl.fence_i
  io.imem.resp.ready := !ctrl_stalld || csr.io.interrupt

  io.imem.btb_update.valid := mem_reg_valid && !mem_npc_misaligned && mem_wrong_npc && ((mem_ctrl.branch && mem_br_taken) || mem_ctrl.jalr || mem_ctrl.jal) && !take_pc_wb
  io.imem.btb_update.bits.isJump := mem_ctrl.jal || mem_ctrl.jalr
  io.imem.btb_update.bits.isReturn := mem_ctrl.jalr && mem_reg_inst(19,15) === BitPat("b00??1")
  io.imem.btb_update.bits.pc := mem_reg_pc
  io.imem.btb_update.bits.target := io.imem.req.bits.pc
  io.imem.btb_update.bits.br_pc := mem_reg_pc
  io.imem.btb_update.bits.prediction.valid := mem_reg_btb_hit
  io.imem.btb_update.bits.prediction.bits := mem_reg_btb_resp

  io.imem.bht_update.valid := mem_reg_valid && mem_ctrl.branch && !take_pc_wb
  io.imem.bht_update.bits.pc := mem_reg_pc
  io.imem.bht_update.bits.taken := mem_br_taken
  io.imem.bht_update.bits.mispredict := mem_wrong_npc
  io.imem.bht_update.bits.prediction := io.imem.btb_update.bits.prediction

  io.imem.ras_update.valid := mem_reg_valid && io.imem.btb_update.bits.isJump && !mem_npc_misaligned && !take_pc_wb
  io.imem.ras_update.bits.returnAddr := mem_int_wdata
  io.imem.ras_update.bits.isCall := mem_ctrl.wxd && mem_waddr(0)
  io.imem.ras_update.bits.isReturn := io.imem.btb_update.bits.isReturn
  io.imem.ras_update.bits.prediction := io.imem.btb_update.bits.prediction

  io.fpu.valid := !ctrl_killd && id_ctrl.fp
  io.fpu.killx := ctrl_killx
  io.fpu.killm := killm_common
  io.fpu.inst := id_inst
  io.fpu.fromint_data := ex_rs(0)
  io.fpu.dmem_resp_val := dmem_resp_valid && dmem_resp_fpu
  io.fpu.dmem_resp_data := io.dmem.resp.bits.data_word_bypass
  io.fpu.dmem_resp_type := io.dmem.resp.bits.typ
  io.fpu.dmem_resp_tag := dmem_resp_waddr

  io.dmem.req.valid     := ex_reg_valid && ex_ctrl.mem
  io.dmem.req.bits.kill := killm_common || mem_xcpt
  io.dmem.req.bits.cmd  := ex_ctrl.mem_cmd
  io.dmem.req.bits.typ  := ex_ctrl.mem_type
  io.dmem.req.bits.phys := Bool(false)
  io.dmem.req.bits.addr := Cat(vaSign(ex_rs(0), alu.io.adder_out), alu.io.adder_out(vaddrBits-1,0)).toUInt
  io.dmem.req.bits.tag := Cat(ex_waddr, ex_ctrl.fp)


  io.dmem.req.bits.data := Mux(mem_ctrl.fp, io.fpu.store_data,
  				Mux(mem_reg_dr_do_xor, mem_reg_dr_rand_data, mem_reg_rs2))
		

  require(coreDCacheReqTagBits >= 6)
  io.dmem.invalidate_lr := wb_xcpt

  io.rocc.cmd.valid := wb_rocc_val
  io.rocc.exception := wb_xcpt && csr.io.status.xs.orR
  io.rocc.s := csr.io.status.prv.orR // should we just pass all of mstatus?
  io.rocc.cmd.bits.inst := new RoCCInstruction().fromBits(wb_reg_inst)
  io.rocc.cmd.bits.rs1 := wb_reg_wdata
  io.rocc.cmd.bits.rs2 := wb_reg_rs2

  if (enableCommitLog) {
    val pc = Wire(SInt(width=64))
    pc := wb_reg_pc
    val inst = wb_reg_inst
    val rd = RegNext(RegNext(RegNext(id_waddr)))
    val wfd = wb_ctrl.wfd
    val wxd = wb_ctrl.wxd
    val has_data = wb_wen && !wb_set_sboard
    val priv = csr.io.status.prv

    when (wb_valid) {
      when (wfd) {
        printf ("%d 0x%x (0x%x) f%d p%d 0xXXXXXXXXXXXXXXXX\n", priv, pc, inst, rd, rd+UInt(32))
      }
      .elsewhen (wxd && rd =/= UInt(0) && has_data) {
        printf ("%d 0x%x (0x%x) x%d 0x%x\n", priv, pc, inst, rd, rf_wdata)
      }
      .elsewhen (wxd && rd =/= UInt(0) && !has_data) {
        printf ("%d 0x%x (0x%x) x%d p%d 0xXXXXXXXXXXXXXXXX\n", priv, pc, inst, rd, rd)
      }
      .otherwise {
        printf ("%d 0x%x (0x%x)\n", priv, pc, inst)
      }
    }

    when (ll_wen && rf_waddr =/= UInt(0)) {
      printf ("x%d p%d 0x%x\n", rf_waddr, rf_waddr, rf_wdata)
    }
  }
  else {
    printf("C%d: %d [%d] pc=[%x] W[r%d=%x][%d] R[r%d=%x] R[r%d=%x] inst=[%x] DASM(%x)\n",
         io.host.id, csr.io.time(32,0), wb_valid, wb_reg_pc,
         Mux(rf_wen, rf_waddr, UInt(0)), rf_wdata, rf_wen,
         wb_reg_inst(19,15), Reg(next=Reg(next=ex_rs(0))),
         wb_reg_inst(24,20), Reg(next=Reg(next=ex_rs(1))),
         wb_reg_inst, wb_reg_inst)
  }

  def checkExceptions(x: Seq[(Bool, UInt)]) =
    (x.map(_._1).reduce(_||_), PriorityMux(x))

  def checkHazards(targets: Seq[(Bool, UInt)], cond: UInt => Bool) =
    targets.map(h => h._1 && cond(h._2)).reduce(_||_)

  def vaSign(a0: UInt, ea: UInt) = {
    // efficient means to compress 64-bit VA into vaddrBits+1 bits
    // (VA is bad if VA(vaddrBits) != VA(vaddrBits-1))
    val a = a0 >> vaddrBits-1
    val e = ea(vaddrBits,vaddrBits-1)
    Mux(a === UInt(0) || a === UInt(1), e =/= UInt(0),
    Mux(a.toSInt === SInt(-1) || a.toSInt === SInt(-2), e.toSInt === SInt(-1),
    e(0)))
  }

  class Scoreboard(n: Int)
  {
    def set(en: Bool, addr: UInt): Unit = update(en, _next | mask(en, addr))
    def clear(en: Bool, addr: UInt): Unit = update(en, _next & ~mask(en, addr))
    def read(addr: UInt): Bool = r(addr)
    def readBypassed(addr: UInt): Bool = _next(addr)

    private val r = Reg(init=Bits(0, n))
    private var _next = r
    private var ens = Bool(false)
    private def mask(en: Bool, addr: UInt) = Mux(en, UInt(1) << addr, UInt(0))
    private def update(en: Bool, update: UInt) = {
      _next = update
      ens = ens || en
      when (ens) { r := _next }
    }
  }
  if(debugRocketCore){
    printf("\n\n=====Status in Rocket(begin)=====\n")
      when(io.imem.resp.valid){
      printf("\t>>>ID stage(begin)\n")
      printf("\tid_inst:\t%x\n",id_inst)
      printf("\tms:\t%x\n",ms)
      printf("\tctrl_stalld:\t%x\n",ctrl_stalld)
      printf("\tid_dr_is_pop:\t%x\n",id_dr_is_pop)
      printf("\tid_dr_is_stack_managing:\t%x\n",id_dr_is_stack_managing)
      printf("\tid_ctrl.csr:\t%x\n",id_ctrl.csr)
      printf("\tCSR.W:\t%x\n",CSR.W)
      printf("\tid_csr_addr:\t%x\n",id_csr_addr)
/*
      printf("\tio.kcache_fase_path_in.valid:\t%x\n",io.kcache_fast_path_in.valid)
      printf("\tio.kcache_fase_path_in.id:\t%x\n",io.kcache_fast_path_in.bits.id)
      printf("\tio.kcache_fast_path_out.valid:\t%x\n",io.kcache_fast_path_out.valid)
      printf("\tio.kcache_fast_path_out.id:\t%x\n",io.kcache_fast_path_out.bits.id)
      printf("\tid_dr_gast_path_match:\t%x\n",id_dr_fast_path_match)
*/    
      when(ms =/= ms_ready){
	printf("\tid_dr_no_ms_in_pipeline:\t%x\n",id_dr_no_ms_in_pipeline)
	printf("\tio.status_stack.cannot_pop:\t%x\n",io.status_stack.cannot_pop)
	printf("\tio.status_stack.cannot_push:\t%x\n",io.status_stack.cannot_push)
	printf("\tio.status_stack.handling_pop:\t%x\n",io.status_stack.handling_pop)
	printf("\tio.status_stack.handling_push:\t%x\n",io.status_stack.handling_push)
      }
      when(!o_stalld & ctrl_stalld){
        printf("\tstall due to dr detected\n")
	printf("\tid_dr_stall:\t%x\n",id_dr_stall)
	printf("\tid_dr_is_stack_managing:\t%x\n",id_dr_is_stack_managing)

        printf("\tnum_id_dr_stall:\t%x\n",num_id_dr_stall)
        printf("\tid_dr_xcpt:\t%x\n",id_dr_xcpt)

      }
      when( id_dr_need_global_key){
        printf("\t\t=====inst at ID stage needs a global mask [begin]=====\n")
          printf("\t\tid_dr_is_mmgl:\t%x\n",id_dr_is_mmgl)
          printf("\t\tid_dr_id_decoded_match:\t%x\n",id_dr_class_id_match)
          printf("\t\tMMsrc:\t%x\n",getMMSrc(getMMIDs(id_inst)))
          printf("\t\tid_dr_id_decoded:\t%x\n",id_dr_id_decoded)
          printf("\t\tid_dr_key:\t%x\n",id_dr_key)
          printf("\t\tid from q:\t%x\n",id_dr_class_id_from_global_q)
          printf("\t\tq valid:\t%x\n",id_dr_global_q.valid)
          printf("\t\tio.dr_mask.ready:\t%x\n",io.dr_mask.ready)
          printf("\t\tio.imem.resp.valid:\t%x\n",io.imem.resp.valid)

    }
    when(id_dr_need_dyn_key){
      printf("\t\t=====inst at ID stage needs a local mask [begin]=====\n")
        printf("\t\tid_dr_id_decoded:\t%x\n",id_dr_id_decoded)
        printf("\t\tio.imem.resp.valid:\t%x\n",io.imem.resp.valid)
        
        printf("\t\t=====inst at ID stage needs a local mask  [end]=====\n")
    }
/*
	for(i <- 0 until id_raddr.size){
          val do_bypass = id_bypass_src(i).reduce(_||_)
          val bypass_src = PriorityEncoder(id_bypass_src(i))
          printf("\t\tdo_bypass:\t%x\n",do_bypass)
          for(j <- 0 until 2){
	    printf("\t\tbyass_src(%d):\t%x\n",UInt(j),bypass_src(j))
	    printf("\t\tid_bypass_src(%d)(%d):\t%x\n",UInt(i),UInt(j),id_bypass_src(i)(j))
	  }
	}
*/
    printf("\tID stage( end )\n")
    }
    when(ex_reg_valid){
      printf("\tEX stage(begin)\n")


      printf("\t\tex_reg_valid:\t%x\n",ex_reg_valid)
      printf("\t\tex_reg_inst:\t%x\n",ex_reg_inst) 
/*
      for (i <- 0 until id_raddr.size){
        printf("\t\tex_reg_rs_bypass(%d):\t%x\n",UInt(i),ex_reg_rs_bypass(UInt(i)))
        for(j <- 0 until 1){	
	  printf("\t\tex_reg_rs_lsb(%d)(%d):\t%x\n",UInt(i),UInt(j),ex_reg_rs_lsb(i)(j))
	}
	printf("\t\tex_rs(%d):\t%x\n",UInt(i),ex_rs(UInt(i)))
      }


	printf("\t\tex_op1:\t%x\n",ex_op1)
	printf("\t\tex_op2:\t%x\n",ex_op2)
*/
      when(ex_reg_dr_need_global_key  || ex_reg_dr_need_dyn_key){
        printf("\t\t=====inst at EX stage needs a mask [begin]=====\n")
          printf("\t\tex_reg_dr_id:\t%x\n",ex_reg_dr_id)
          printf("\t\tex_reg_valid:\t%x\n",ex_reg_valid)
          printf("\t\tex_reg_dr_st_key:\t%x\n",ex_reg_dr_st_key)
          printf("\t\tex_dr_dyn_key:\t%x\n",ex_dr_dyn_key)
	  printf("\t\talu.io.adder_out:\t%x\n",alu.io.adder_out)
	  printf("\t\tex_reg_dr_fast_path_match:\t%x\n",ex_reg_dr_fast_path_match)
	  printf("\t\tkey_mask:\t%x\n",key_mask)
	  printf("\t\tio.kcache_fast_path_in.bits.key:\t%x\n",io.kcache_fast_path_in.bits.key)
          printf("\t\t=====inst at EX stage needs a mask   [end]=====\n")
      }

    printf("\tEX stage( end )\n")
    }
    when(mem_reg_valid){
      printf("\tME stage(begin)\n")
      printf("\t\tmem_reg_valid:\t%x\n",mem_reg_valid)
      printf("\t\tmem_reg_inst:\t%x\n",mem_reg_inst)
      when(mem_reg_dr_need_global_key || mem_reg_dr_need_dyn_key){
        printf("\t\t=====inst at MEM stage needs a mask [begin]=====\n")
          printf("\t\tmem_reg_dr_key:\t%x\n",mem_reg_dr_key)
          printf("\t\t=====inst at MEM stage needs a mask [  end]=====\n")
      }

    when(io.dmem.req.fire()){
      printf("mem_reg_dr_rand_data:\t%x\n",mem_reg_dr_rand_data);
      printf("mem_reg_rs2:\t%x\n",mem_reg_rs2);
      printf("mem_reg_dr_bitmaek:\t%x\n",mem_reg_dr_key)
    }

    printf("\tME stage( end )\n")
    }
    when(wb_reg_valid){
      printf("\tWB stage(begin)\n")
      printf("\t\twb_reg_valid:\t%x\n",wb_reg_valid)
      printf("\t\twb_reg_inst:\t%x\n",wb_reg_inst)
      when(wb_reg_dr_need_global_key){
        printf("\t\t=====inst at WB stage needs a mask [begin]=====\n")
          printf("\t\twb_reg_dr_is_mmgl:\t%x\n",wb_reg_dr_is_mmgl)
          printf("\t\twb_reg_dr_key:\t%x\n",wb_reg_dr_key)
          printf("\t\twb_set_sboard:\t%x\n",wb_set_sboard)
          printf("\t\trf_wen:\t%x\n",rf_wen)
          printf("\t\twb_mask:\t%x\n",wb_mask)
          printf("\t\twb_reg_dr_key:\t%x\n",wb_reg_dr_key)
          printf("\t\tio.dmem.resp.bits.data:\t%x\n",io.dmem.resp.bits.data)
          printf("\t\twb_wdata_unmasked:\t%x\n",wb_wdata_unmasked)
          printf("\t\trf_wdata:\t%x\n",rf_wdata)
          printf("\t\t=====inst at WB stage needs a mask [  end]=====\n")
      }
    when(isMM(wb_reg_inst)){
      printf("mm* in wb\n")
        printf("wb_reg_valid:\t%x\n",wb_reg_valid)
        printf("MMIDs:\t%x\n",getMMIDs(wb_reg_inst))
        printf("DestID:\t%x\n",getMMDest(getMMIDs(wb_reg_inst)))
        printf("SrcID:\t%x\n",getMMSrc(getMMIDs(wb_reg_inst)))
        printf("csr.io.rw.dr_key:\t%x\n",csr.io.rw.dr_key)
    }

    printf("\tWB stage( end )\n")
    }

      when(rf_wen && ll_wen){
        printf("ll_wen && rf_wen\n")
          printf("ll_mask:\t%x\n",ll_mask)
          printf("addr,mask:\t%x,%x\n",ll_waddr,ll_mask)
	  printf("data:\t%x\n",ll_wdata_unmasked)
      }


    
    when(io.dmem.resp.fire()){
      printf("\tio.dmem.resp.fire()\n")
      printf("\t\tneedMask(io.dmem.resp.bits.cmd):\t%x\n",needsMask(io.dmem.resp.bits.cmd))
      printf("\t\tll_wen_pre:\t%x\n",ll_wen_pre)
      printf("\t\tll_mask:\t%x\n",ll_mask)
      printf("\t\tcmd:\t%x\n",io.dmem.resp.bits.cmd)
      printf("\t\tdata:\t%x\n",io.dmem.resp.bits.data)
      printf("\t\thas_data:\t%x\n",io.dmem.resp.bits.has_data)
      printf("\t\tdata_word_bypass:\t%x\n",io.dmem.resp.bits.data_word_bypass)
      printf("\t\tstore_data:\t%x\n",io.dmem.resp.bits.store_data)
      printf("\t\tdcache_bypass_data:\t%x\n",dcache_bypass_data)
      for(i<-0 until 4){
        printf("\t\tbypass_sources(%d)(%d):\t%x\n",UInt(i),UInt(0),Vec(bypass_sources.map(_._1))(i))
        printf("\t\tbypass_sources(%d)(%d):\t%x\n",UInt(i),UInt(1),Vec(bypass_sources.map(_._2))(i))
        printf("\t\tbypass_sources(%d)(%d):\t%x\n",UInt(i),UInt(2),Vec(bypass_sources.map(_._3))(i))
      }

    }

    printf("\n\n=====Status in Rocket( end )=====\n")

  }
    val cycles = Reg(init=UInt(0,width=64))
    val num_fast_match = Reg(init=UInt(0,width=64))
    val num_slow_match = Reg(init=UInt(0,width=64))
    val num_need_static_id = Reg(init=UInt(0,width=64))
    cycles := cycles + UInt(1)
    when(id_dr_fast_path_match & !ctrl_stalld & id_dr_need_global_key){
      num_fast_match := num_fast_match + UInt(1)
    }
    when(id_dr_class_id_match && !id_dr_fast_path_match && !ctrl_stalld && id_dr_need_global_key){
      num_slow_match := num_slow_match + UInt(1)
    }
    when(!ctrl_killd & id_dr_need_global_key){
      num_need_static_id := num_need_static_id + UInt(1)
      printf("Cycles:\t%x\n",cycles)
      printf("[static]num_fast_match:\t%d\n",num_fast_match)
      printf("[static]num_slow_match:\t%d\n",num_slow_match)
      printf("num_need_static_id:\t%d\n",num_need_static_id)
    }


}
