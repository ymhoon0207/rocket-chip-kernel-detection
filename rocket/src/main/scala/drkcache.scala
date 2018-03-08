


package rocket


import Chisel._
import uncore._
import junctions._
import cde.{Parameters, Field}
import Util._

trait HasMetaDRParameters{


  implicit val p: Parameters
  val drEqClassIdBits = 11
  val drTempBufLen = 16
  val drSnoopedGrantBufLen = 16
  val drDryMasking = false
  val drKeyBits = p(XLen)
  val drEqClassIdxBits = drEqClassIdBits - 3


  //debug flags
  val debugDRKCache = false
  val debugCSRFile = false
  val debugRocketCore = false
  val debugDRKStack = false

  
  //methods
  def isLoadUnmask(inst: Bits): Bool = inst(1,0) === UInt(0x3) && inst(6,2) === UInt(0x02)
  def isStoreMask(inst: Bits): Bool = inst(1,0) === UInt(0x3) && inst(6,2) === UInt(0x0A)
  def isMMGL(inst: Bits): Bool = inst(1,0) === UInt(0x3) && inst(6,2) === UInt(0x16) && inst(11,7) === UInt(0x1) 
  def isMMLL(inst: Bits): Bool = inst(1,0) === UInt(0x3) && inst(6,2) === UInt(0x16) && inst(11,7) === UInt(0x0) 
  def isMM(inst: Bits): Bool = isMMGL(inst) || isMMLL(inst)
  
  def getDRIdField(inst: Bits): Bits = Mux(isMMGL(inst),Cat(UInt(0,width=1),inst(22,12)),(Mux(isLoadUnmask(inst),inst(31,20),Cat(inst(31,25),inst(11,7)))))

  def getMMIDs(inst: Bits): Bits = inst(31,12)
  def getMMDest(ids: Bits): UInt = ids(19,11)
  def getMMSrc(ids: Bits): UInt = ids(10,0)

  def isForStack(inst: Bits): Bool = isMMGL(inst)
  def isDRLocal(inst: Bits): Bool = getDRIdField(inst)(11) || isMMLL(inst)
  //this returns src id if mmgl
  def getDRId(inst: Bits): Bits = Mux(isMMGL(inst),getMMSrc(getMMIDs(inst)),getDRIdField(inst)(10,0))

  def needsGlobalKey(inst: Bits): Bool  = isMMGL(inst) || ((isStoreMask(inst) || isLoadUnmask(inst)) && (!isDRLocal(inst)))
  def needsLocalKey(inst:Bits): Bool = isMMLL(inst) || ((isStoreMask(inst) || isLoadUnmask(inst)) && (isDRLocal(inst)))

}

trait HasDRParameters extends HasL1CacheParameters
with HasMetaDRParameters {

  val lineBits = outerDataBeats * outerDataBits
  require(coreInstBits == 32,"coreInstBits is assumed to be 32")
  require(outerDataBeats == 4,"outerDataBeats is assumed to be 4")
  require(outerDataBits == 128,"outerDataBits is assumed to be 128")
  require(fetchWidth == 1,"single issue only")

  val numInstPerBeat = outerDataBits / coreInstBits

  val enableDebugCounters = true

  val sizeOfKCacheInByte = 1 * 1024
  val sizeOfLineInByte = outerDataBeats * outerDataBits / 8 
  val numLines = sizeOfKCacheInByte / sizeOfLineInByte
  val tagIdxBits = log2Up(numLines) 

}


abstract class DRModule(implicit val p: Parameters) extends Module
with HasDRParameters
abstract class DRBundle(implicit val p: Parameters) extends junctions.ParameterizedBundle()(p)
with HasDRParameters

class DRIdMask(implicit p:Parameters) extends DRBundle()(p) {
  val idmask = UInt(width=coreDataBits + drEqClassIdBits)
}

class DRKEqClassId(implicit p:Parameters) extends DRBundle()(p) {
  val eq_class_id = UInt(width = drEqClassIdBits)
  //length of bufferw which could be removed later
}
class DRKCacheLine(implicit p:Parameters) extends DRBundle()(p) {
  val data = Bits(width=outerDataBits*outerDataBeats)
}
class DRKCacheSnoopInner(implicit p: Parameters) extends DRModule {

  val io = new Bundle {
    val active = Bool(INPUT)
    val fe_next_insn = Valid(
      new Bundle {
        val data = UInt(OUTPUT)
      }
    ).flip
    val s_resp = Valid(new FrontendResp).flip
    val cache_req = Decoupled(
      new Bundle{
        val eq_class_id = UInt(OUTPUT)
      }
    )
    val cache_resp = Decoupled(
      new Bundle{
        val idmask = UInt(OUTPUT)
      }
    ).flip
    val matched_at_id = Bool(INPUT)
    val core_req = Decoupled(
      new Bundle{
        val idmask = UInt(OUTPUT)
      }
    )
    
  }

  val wire_snooped_inst = io.s_resp.bits.data(0)
  val inst_needs_global_key = needsGlobalKey(wire_snooped_inst)
  val reg_prev_id = Reg(init=UInt(0))
  val reg_valid = Reg(init=Bool(false))
  reg_valid := io.s_resp.valid & inst_needs_global_key
  when(io.s_resp.valid & inst_needs_global_key){
    reg_prev_id := getDRId(io.s_resp.bits.data(0))
  }
  val q_s_resp = Module (new Queue (UInt(width=drEqClassIdBits+1),16))
  val wire_snooped_id = getDRId(io.s_resp.bits.data(0)) 
  val wire_snooped_pc = io.s_resp.bits.pc
  val reg_snooped_pc_prev = Reg(next=wire_snooped_pc)
  q_s_resp.io.enq.valid := io.s_resp.valid & inst_needs_global_key & (!reg_valid || (wire_snooped_pc =/= reg_snooped_pc_prev)) & io.active
  q_s_resp.io.enq.bits := Cat(isForStack(io.s_resp.bits.data(0)),getDRId(io.s_resp.bits.data(0)))


  val buf_eq_class_id = Reg(UInt())
  val q_idmask_core = Module(new Queue (new DRIdMask()(p),16))
  q_idmask_core.io.enq <> io.cache_resp
  io.core_req <> q_idmask_core.io.deq
  //io.core_req <> io.cache_resp

 
  val reg_prev_pc= Reg(UInt())
  when(io.cache_req.fire()){
    reg_prev_pc := io.s_resp.bits.pc
  
  }
 
  val prev_pc = Reg(init=UInt(0))
  prev_pc := io.s_resp.bits.pc
  val cur_pc = io.s_resp.bits.pc
  val this_insn_sent = Reg(init=Bool(false))
  val next_needs_global_key = needsGlobalKey(io.fe_next_insn.bits.data)
  val this_insn_valid = io.s_resp.valid & inst_needs_global_key & io.active & !io.matched_at_id
  val next_insn_valid = io.fe_next_insn.valid & next_needs_global_key & io.active
  this_insn_sent := !this_insn_valid & next_insn_valid & io.cache_req.fire()
  
  io.cache_req.bits.eq_class_id := Mux(this_insn_valid, getDRId(io.s_resp.bits.data(0)), getDRId(io.fe_next_insn.bits.data))
  io.cache_req.valid := this_insn_valid || next_insn_valid
  //io.cache_req.bits.eq_class_id := q_s_resp.io.deq.bits(drEqClassIdBits-1,0)
  //io.cache_req.valid := q_s_resp.io.deq.valid
  //q_s_resp.io.deq.ready := io.cache_req.ready


   
  val num_this_insn = Reg(init=UInt(0,width=64))
  val num_next_insn = Reg(init=UInt(0,width=64))
  when(this_insn_valid & io.cache_req.fire()){
    num_this_insn := num_this_insn + UInt(1)
    printf("num_this_insn:\t%d\n",num_this_insn)
  }
  when(next_insn_valid & !this_insn_valid & io.cache_req.fire()){
    num_next_insn := num_next_insn + UInt(1)
    printf("num_next_insn:\t%d\n",num_next_insn)
  }


  if(debugDRKCache){
    
    val p_cond = io.core_req.fire() || io.cache_req.fire() || io.cache_resp.fire() ||
			q_s_resp.io.enq.fire() || q_s_resp.io.deq.fire()
    printf("\n\n=====Status in DRKCacheSnoopInner (begin)=====\n")
    printf("\tq_s_resp.io.count:\t%d\n",q_s_resp.io.count)
    
    when(q_s_resp.io.enq.fire()){
      printf("\tq_s_resp.io.enq.fire() with\t%x\n",q_s_resp.io.enq.bits)
    }
    when(q_s_resp.io.deq.fire()){
      printf("\tq_s_resp.io.deq.fire() with\t%x\n",q_s_resp.io.deq.bits)
    }
    when(io.core_req.fire()){
      printf("\tcore_req fires with class idmask:\t%x\n",io.core_req.bits.idmask)
    }
    when(io.cache_req.fire()){
      printf("\tcache_req fires with class id:\t%x\n",io.cache_req.bits.eq_class_id)
    }
    when(io.cache_resp.fire()){
      printf("\tcache_resp fires with idmask:\t%x\n",io.cache_resp.bits.idmask)
    }
    /*
    when(q_idmask_core.io.enq.fire()){
      printf("enq to q_idmask_core:\t%x\n",q_idmask_core.io.enq.bits.idmask)
    }
    when(q_idmask_core.io.deq.fire()){
      printf("deq from q_idmask_core:\t%x\n",q_idmask_core.io.deq.bits.idmask)
    }
    */
    printf("\n\n=====Status in DRKCacheSnoopInner ( end )=====\n")
  }
}

class DRKCacheSnoopOuter(implicit p: Parameters) extends DRModule {
//This module snoops the outer access for L1 I$ and generates corresponding refill
//request to the other modules in DRKCache
  println("----------hgmoon------------------------")
  println("\t\tEntering SnoopOuter")
  println("\t\tcoreInstBits:\t" + coreInstBits)
  println("\t\touterDataBeats:\t" + outerDataBeats)
  println("\t\tdrDryMasking:\t" + drDryMasking)
  println("----------hgmoon------------------------")

  val io = new Bundle{
    //signals snooped
    //val s_acquire = new Valid(new Acquire).flip
    //val s_probe = new Valid(new Probe).flip
    //val s_release = new Valid(new Release).flip
    val active = Bool(INPUT)
    val s_grant = Valid(new Grant).flip
    //val s_finish = new Valid(new Finish).flip    
     
    
    val req = Decoupled(new DRKEqClassId()(p))
    val ready = Bool(OUTPUT)
  }

  //datapath

  //TODO: This Q would be able to be removed
  val q_s_grant = Module(new Queue (io.s_grant.bits,drSnoopedGrantBufLen))
  q_s_grant.io.enq.bits := io.s_grant.bits
  q_s_grant.io.enq.valid := io.s_grant.valid & io.active


  val insts = Wire(Vec.fill(numInstPerBeat){Bits(width=coreInstBits)})
  for (i <-0 until numInstPerBeat){
    insts(i) := q_s_grant.io.deq.bits.data(coreInstBits * (i+1) - 1,coreInstBits * i) 
  }
  //XXX
  io.ready := Bool(true)

  val buf_ids = Reg(Vec.fill(numInstPerBeat){Bits(width=drEqClassIdBits)})
  val buf_ids_valids = Reg(init=Bits(0,width=numInstPerBeat))
  val buf_ids_passed = Reg(init=Bits(0,width=numInstPerBeat))

  val buf_ids_ready = buf_ids_valids === buf_ids_passed
  q_s_grant.io.deq.ready := buf_ids_ready

  when(q_s_grant.io.deq.fire()){
    for(i <- 0 until numInstPerBeat){
      buf_ids(i) := getDRId(insts(i)) //this should be updated with the encoding
      when(needsGlobalKey(insts(i))){
        buf_ids_valids(i) := Bits(1) 
      }.otherwise{
        buf_ids_valids(i) := Bits(0)
      }
    }
  }

  val q_ids = Module(new Queue(Bits(width=drEqClassIdBits),drSnoopedGrantBufLen * numInstPerBeat))
  val left = buf_ids_valids & ~(buf_ids_passed)
  val pos_left = 	PriorityMux(left, UInt(0) :: UInt(1) :: UInt(2) :: UInt(3) :: Nil)
  
  q_ids.io.enq.bits := buf_ids(pos_left)
  q_ids.io.enq.valid := !buf_ids_ready

  when(q_s_grant.io.deq.fire()){
    buf_ids_passed := UInt(0)
  }.elsewhen(q_ids.io.enq.fire()){
    buf_ids_passed := buf_ids_passed | UIntToOH(pos_left)
  }

  q_ids.io.deq.ready := io.req.ready
  io.req.valid := q_ids.io.deq.valid
  io.req.bits.eq_class_id := q_ids.io.deq.bits

}


class DRKFetcher(implicit p: Parameters) extends DRModule {
 
  val io = new Bundle {
    val config = new Bundle{
      val key_table_base = UInt(INPUT)
    }
    val req = Decoupled(
      new Bundle {
        val eq_class_idx = UInt(OUTPUT)
      }
    ).flip
    val resp = Decoupled(
      new Bundle {
        val keys = UInt(OUTPUT)
      }
    )

    val tl = new ClientUncachedTileLinkIO

  }

/*
  debug counters
*/

  if(enableDebugCounters) {
    val count_accessed = Reg(init=UInt(0,width=32))
    when(io.req.fire()) {
      count_accessed := count_accessed + UInt(1)
    }
  }


  val f_ready :: f_valid :: f_wait :: f_done :: Nil = Enum(UInt(),4)
  val state_fetch = Reg(init=f_ready)

  val client_xact_id = Reg(UInt())

  val cnt_gnt = Reg(UInt(width = 2))
  val vec_entry = Reg(Vec.fill(outerDataBeats){Bits()})
  val addr_block = Reg(UInt())
  val buf_eq_class_idx = Reg(UInt())
  when(state_fetch === f_ready) {
    when(io.req.fire()) {
      state_fetch := f_valid
      addr_block := (io.config.key_table_base + Cat(io.req.bits.eq_class_idx, UInt(0,width=(3+3))))(31,6)
      client_xact_id := UInt(0)
      buf_eq_class_idx := io.req.bits.eq_class_idx     
    }
  }
  when(state_fetch === f_valid) {
    when(io.tl.acquire.fire()) {
      state_fetch := f_wait
      cnt_gnt := UInt(0)
    }
  }
  when(state_fetch === f_wait) {
    when(io.tl.grant.fire() && io.tl.grant.bits.client_xact_id === client_xact_id) {
      cnt_gnt := cnt_gnt + UInt(1)
      vec_entry(cnt_gnt) := io.tl.grant.bits.data
      when(cnt_gnt === UInt(outerDataBeats-1)) {
        state_fetch := f_done
      }
    }
  }
  when(state_fetch === f_done) {
    when(io.resp.fire()) {
      state_fetch := f_ready
    }
  }

  
  io.req.ready := state_fetch === f_ready
  io.resp.bits.keys := Cat(vec_entry(UInt(3))(127,0),vec_entry(UInt(2))(127,0),vec_entry(UInt(1))(127,0),vec_entry(UInt(0))(127,0))
  io.resp.valid := state_fetch === f_done

  io.tl.acquire.valid := state_fetch === f_valid
  io.tl.acquire.bits := Bundle(GetBlock (
                                      client_xact_id = client_xact_id,
				      addr_block = addr_block,
				      alloc = Bool(true)
				      ))(params)
				     
  io.tl.grant.ready := state_fetch === f_wait

  if(debugDRKCache){ 
    printf("\n\n=====Status in DRKFetcher(begin)=====\n")
      printf("io.tl.acquire.valid:\t%x\n",io.tl.acquire.valid)
      printf("io.tl.grant.ready:\t%x\n",io.tl.grant.ready)
      when(io.tl.acquire.fire()){
	printf("\tAcquire sent\n")
	  printf("\t\tbase_addr:\t%x\n",Cat(io.tl.acquire.bits.addr_block,UInt(0,width=6)))
	  printf("\t\txact_id:\t%x\n",io.tl.acquire.bits.client_xact_id)
      }
    when(io.tl.grant.fire()){
      printf("\tGrant received\n")
	printf("\t\tdata:\t%x\n",io.tl.grant.bits.data)
	printf("\t\txact_id:\t%x\n",io.tl.grant.bits.client_xact_id)
    }
    printf("=====Status in DRKFetcher( end )=====\n\n\n")
  } 

}

class DRKMeta(implicit p: Parameters) extends DRBundle {
  val valid = Bool(false)
}

class DRKCacheCore(implicit p: Parameters) extends DRModule {
  val io = new Bundle{
    val active = Bool(INPUT)
    val cache_req = Decoupled(
      new Bundle{
        val eq_class_id = UInt(OUTPUT)
      }
    ).flip
    val cache_resp = Decoupled(
      new Bundle{
        val idmask = UInt(OUTPUT)
      }
    )

    val cpu_fast_path_out = Valid(
      new Bundle{
        val id = UInt(OUTPUT)
	val key = UInt(OUTPUT)
      }
    )
    val cpu_fast_path_in = Valid(
      new Bundle{
        val matched_at_id = Bool(OUTPUT)
        val id = UInt(OUTPUT)
      }
    ).flip
    val prefetch_req = Decoupled(new DRKEqClassId()).flip
    val fetch_req = Decoupled(
        new Bundle {
          val eq_class_idx = UInt(OUTPUT)
        }
      )
    val fetch_resp = Decoupled(new DRKCacheLine()).flip
  }


  //early defs
  val mh_reg_is_prefetch = Reg(UInt())
  val mh_reg_id = Reg(UInt())
  val mh_idx = mh_reg_id(drEqClassIdBits-1,3)
  val mh_ready :: mh_valid :: mh_wait :: Nil = Enum(UInt(),3)
  val mh_state = Reg(init=mh_ready)
  val s2_any_tag_hit = Wire(init=Bool(false))
  val mh_reg_id_core_buf = Reg(UInt())
  val mh_reg_id_core_buf_valid = Reg(init=Bool(false))


  //


  val tag_array = Reg(init=Vec.fill(numLines){Bits(0,width=drEqClassIdxBits + 1)})
  val data_array = Mem(numLines, Bits(width=lineBits))

  val initFill = Reg(init=Bool(true))
  val initcount = Reg(init=UInt(0,width=log2Up(numLines)))
  val victimIdx = Wire(UInt())
  val lfsr = Cat(LFSR16(io.fetch_req.fire()),LFSR16(io.fetch_req.fire()))(log2Up(numLines)-1,0)

  //val replacer = p(Replacer)()(numLines) 
  initFill := initcount =/= UInt(numLines-1)
  victimIdx := Mux(initFill,initcount,lfsr)

  when(io.fetch_req.fire() & initcount =/= UInt(numLines-1)){
    initcount := initcount + UInt(1)
  }

  val s0_id_to_match = Module(new Queue(UInt(width=drEqClassIdBits), 1, pipe=true))

  val kcache_refill = io.fetch_resp.fire()
  val stall = Wire(init=Bool(false))
  val s1_id_arb = Module(new Arbiter(UInt(),3))
  s1_id_arb.io.out.ready := !kcache_refill
  val s1_valid = s1_id_arb.io.out.valid
  val s1_id_to_match = s1_id_arb.io.out.bits
  val s1_is_prefetch = io.prefetch_req.fire() 

  s1_id_arb.io.in(0).valid := s0_id_to_match.io.deq.valid
  s1_id_arb.io.in(0).bits := s0_id_to_match.io.deq.bits
  s0_id_to_match.io.deq.ready := s1_id_arb.io.in(0).ready
 

  s1_id_arb.io.in(1).valid := io.cache_req.valid & (!s2_any_tag_hit || mh_state === mh_ready)
  s1_id_arb.io.in(1).bits := io.cache_req.bits.eq_class_id
  io.cache_req.ready := s1_id_arb.io.in(1).ready & (!s2_any_tag_hit || mh_state === mh_ready)

  s1_id_arb.io.in(2).valid := Bool(false)//io.prefetch_req.valid & (!s2_any_tag_hit || mh_state === mh_ready)
  s1_id_arb.io.in(2).bits := io.prefetch_req.bits.eq_class_id
  io.prefetch_req.ready := Bool(false)//s1_id_arb.io.in(2).ready & (!s2_any_tag_hit || mh_state === mh_ready)
  

  val s1_idx_to_match = s1_id_to_match(drEqClassIdBits-1,3)

  val s2_reg_is_prefetch = Reg(next=s1_is_prefetch)
  val s2_reg_id_to_match = Reg(next=s1_id_to_match)
  val s2_reg_tag_match = Reg(init=Vec.fill(numLines)(Bool(false)))
  val s2_reg_valid = Reg(init=Bool(false))
  val kcache_stall = Wire(init=Bool(false))
  s2_reg_valid := !kcache_refill && s1_valid


  when(kcache_refill){
    tag_array(victimIdx) := Cat(Bool(true),mh_idx)
    when(mh_reg_id === s1_id_to_match){
      for(i <- 0 until numLines){
        s2_reg_tag_match(i) := UInt(i) === victimIdx
      }
    }.otherwise{
      for(i <- 0 until numLines){
        s2_reg_tag_match(i) := 
          tag_array(i)(drEqClassIdxBits) === Bits(1) && 
          tag_array(i)(drEqClassIdxBits-1,0) === s1_idx_to_match &&
	  UInt(i) =/= victimIdx //could have been hit..
      }
    }
  }.otherwise{
    for(i <- 0 until numLines){
      s2_reg_tag_match(i) := 
        tag_array(i)(drEqClassIdxBits) === Bits(1) && 
        tag_array(i)(drEqClassIdxBits-1,0) === s1_idx_to_match
    }
  }

  s2_any_tag_hit := s2_reg_tag_match.reduceLeft(_||_)

  val s3_fast_path = Reg(init=Bool(false))
  val s3_reg_hit = Reg(init=Bool(false))
  s3_reg_hit := s2_any_tag_hit || kcache_refill
  val s3_reg_valid = Reg(init=Bool(false))
  s3_reg_valid := s2_reg_valid || kcache_refill
  val s3_reg_is_prefetch = Reg(next=s2_reg_is_prefetch)
  s3_reg_is_prefetch := s2_reg_is_prefetch || (kcache_refill & mh_reg_is_prefetch)
  val s3_reg_kcache_miss = Reg(init=Bool(false))
  val s3_reg_id_to_match = Reg(UInt())
  s3_reg_id_to_match := Mux(kcache_refill, mh_reg_id, s2_reg_id_to_match)
  val s3_id_to_match_offset = s3_reg_id_to_match(2,0)
  val s3_reg_data_line = Reg(init=UInt(width=8*64))
  val raddr = (Mux(kcache_refill, victimIdx + 1, OHToUInt(s2_reg_tag_match)))
  when(kcache_refill){ 
    data_array.write(victimIdx, Cat(io.fetch_resp.bits.data) )
    s3_reg_data_line := io.fetch_resp.bits.data
  }.elsewhen(s2_reg_valid){
    s3_reg_data_line := data_array.read(raddr)
  }
  val s3_data_line_vec = Wire(init=Vec.fill(8)(UInt(width=64)))
 

  for(i <- 0 until 8){
    s3_data_line_vec(i)(63,0) := s3_reg_data_line(64*(i+1)-1,64*i)
  }
  val s3_data_out = Mux1H(UIntToOH(s3_id_to_match_offset),s3_data_line_vec)


  io.cpu_fast_path_out.valid := s2_reg_valid && s2_any_tag_hit && !kcache_refill
  io.cpu_fast_path_out.bits.id := s2_reg_id_to_match
  io.cpu_fast_path_out.bits.key := s3_data_out
  s3_fast_path := io.cpu_fast_path_in.bits.id === s2_reg_id_to_match &
  			io.cpu_fast_path_in.valid 



  //val mh_ready :: mh_valid :: mh_wait :: Nil = Enum(UInt(),3)
  //val mh_state = Reg(init=mh_ready)

  //val mh_reg_id = Reg(UInt())
  //val mh_idx = mh_reg_id(drEqClassIdBits-1,3)
  //val mh_reg_is_prefetch = Reg(UInt())

  //val mh_reg_id_core_buf = Reg(UInt())
  //val mh_reg_id_core_buf_valid = Reg(init=Bool(false))

  s0_id_to_match.io.enq.valid := mh_state =/= mh_ready & !s2_any_tag_hit & s2_reg_valid & !s2_reg_is_prefetch 
  s0_id_to_match.io.enq.bits := s2_reg_id_to_match


  val need_to_buf_s2 = mh_state =/= mh_ready & !s2_any_tag_hit & s2_reg_valid & !s2_reg_is_prefetch
  mh_reg_id_core_buf := Mux(need_to_buf_s2, s2_reg_id_to_match, mh_reg_id_core_buf)
  mh_reg_id_core_buf_valid := Bool(false)/*Mux(mh_state === mh_ready, Bool(false), 
   				Mux(need_to_buf_s2, Bool(true), mh_reg_id_core_buf_valid))*/

  when(!io.active){
    mh_state := mh_ready
  }.elsewhen(mh_state === mh_ready){
    when(mh_reg_id_core_buf_valid){
      mh_reg_id := mh_reg_id_core_buf
      mh_reg_is_prefetch := Bool(false)
      mh_state := mh_valid
    }.elsewhen((!s2_any_tag_hit & s2_reg_valid)){
      mh_reg_id := s2_reg_id_to_match
      mh_reg_is_prefetch := s2_reg_is_prefetch
      mh_state := mh_valid
    }
  }.elsewhen(mh_state === mh_valid){
    when(io.fetch_req.fire()){
      mh_state := mh_wait
    }
  }.elsewhen(mh_state === mh_wait){
    when(io.fetch_resp.fire()){
      mh_state := mh_ready
    }
  }


  io.fetch_req.bits.eq_class_idx := mh_idx
  io.fetch_req.valid := mh_state === mh_valid
  io.fetch_resp.ready := mh_state === mh_wait

  io.cache_resp.valid := (s3_reg_valid & s3_reg_hit & !s3_reg_is_prefetch) & !s3_fast_path
  io.cache_resp.bits.idmask := Cat(s3_reg_id_to_match,s3_data_out)



  if(debugDRKCache){
    val need_print = 	io.cache_resp.fire() || 
    			io.cache_req.fire()  ||
			Bool(true)
    when(need_print){
    printf("\n\n=====Status in DRKCacheCore (begin)=====\n")
      when(io.cache_req.fire()){
        printf("\tcache_req fires\n")
      }
      when(io.cache_resp.fire()){
        printf("\tcache_resp fires\n")
      }
      when(io.fetch_req.fire()){
        printf("\tfetch_req fires with id:\t%x\n",mh_reg_id)
      }
      when(io.fetch_resp.fire()){
        printf("\tfetch_resp fires with id:\t%x\n",mh_reg_id)
	printf("\tline:\t%x\n",io.fetch_resp.bits.data)
      }
      printf("\ts1_valid:\t%x\n",s1_valid)
      printf("\ts1_id_to_match:\t%x\n",s1_id_to_match)
      printf("\ts2_reg_valid:\t%x\n",s2_reg_valid)
      printf("\ts2_reg_id_to_match:\t%x\n",s2_reg_id_to_match)
      printf("\ts2_any_tag_hit:\t%x\n",s2_any_tag_hit)
      printf("\ts3_reg_valid:\t%x\n",s3_reg_valid)
      printf("\ts3_reg_id_to_match:\t%x\n",s3_reg_id_to_match)
      printf("\ts3_reg_hit:\t%x\n",s3_reg_hit)
      printf("\ts3_reg_is_prefetch:\t%x\n",s3_reg_is_prefetch)
      printf("\ts3_fast_path:\t%x\n",s3_fast_path)
      printf("\ts3_data_out:\t%x\n",s3_data_out)
    printf("\n\n=====Status in DRKCacheCore ( end )=====\n")
    }
  }
  val num_kcache_miss = Reg(init=UInt(0,width=64))
  when(io.fetch_req.fire()){
    printf("[static]num_kcache_miss:\t%d\n",num_kcache_miss + UInt(1))
    num_kcache_miss := num_kcache_miss + UInt(1)
  }
}


class DRKCache(implicit p: Parameters) extends DRModule {

  val io = new Bundle{
    val active = Bool(INPUT)
    val static_table_base_out = UInt(OUTPUT)
    val static_table_base_in = UInt(INPUT)
    val fe_next_insn = Valid(
      new Bundle {
        val data = UInt(OUTPUT)
      }
    ).flip
    val cpu = Decoupled(
      new Bundle{
        val idmask = UInt(OUTPUT)
      }
    )
    val cpu_fast_path_out = Valid(
      new Bundle{
        val id = UInt(OUTPUT)
	val key = UInt(OUTPUT)
      }
    )
    val cpu_fast_path_in = Valid(
      new Bundle{
        val matched_at_id = Bool(OUTPUT)
        val id = UInt(OUTPUT)
      }
    ).flip

    val s_resp = Valid(new FrontendResp).flip
    val s_grant = Valid(new Grant).flip

    val mem = new ClientUncachedTileLinkIO

    val config = Vec(16,UInt(INPUT))
    val enable = Bool(OUTPUT)
    val reset_perf_counters = UInt(OUTPUT)

  }
  //configs
  val enable = Bool(true)//io.config(0)(0)
  io.enable := enable

  val mask_table_base_mmio_prev = Reg(next=io.config(1))
  val mask_table_base_mmio = io.config(1)
  val mask_table_base_csr_prev = Reg(next=io.static_table_base_in)
  val mask_table_base_csr = io.static_table_base_in

  val mask_table_base = Reg(UInt())
  when(Bool(true)){
    when(mask_table_base_mmio =/= mask_table_base_mmio_prev){
      mask_table_base := mask_table_base_mmio
    }.elsewhen(mask_table_base_csr_prev =/= mask_table_base_csr){
      mask_table_base := mask_table_base_csr 
    }
  }

  val reset_perf_counters = io.config(2)
  io.reset_perf_counters := reset_perf_counters

  io.static_table_base_out := mask_table_base
  //datapath
  val snoop_inner = Module(new DRKCacheSnoopInner()(p))
  io.cpu.bits.idmask := snoop_inner.io.core_req.bits.idmask
  io.cpu.valid := snoop_inner.io.core_req.valid & enable
  snoop_inner.io.core_req.ready := io.cpu.ready
  snoop_inner.io.active := io.active
  snoop_inner.io.fe_next_insn := io.fe_next_insn
  
  snoop_inner.io.s_resp.valid := io.s_resp.valid & enable
  snoop_inner.io.s_resp.bits := io.s_resp.bits
  snoop_inner.io.matched_at_id := io.cpu_fast_path_in.bits.matched_at_id 

  val core = Module(new DRKCacheCore()(p))
  core.io.cache_req <> snoop_inner.io.cache_req
  core.io.cache_resp <> snoop_inner.io.cache_resp
  core.io.active := io.active
  core.io.cpu_fast_path_in := io.cpu_fast_path_in
  io.cpu_fast_path_out := core.io.cpu_fast_path_out



  val snoop_outer = Module(new DRKCacheSnoopOuter()(p))
  snoop_outer.io.s_grant.bits := io.s_grant.bits
  snoop_outer.io.s_grant.valid := io.s_grant.valid & enable
  snoop_outer.io.active := io.active

  core.io.prefetch_req.valid := snoop_outer.io.req.valid
  core.io.prefetch_req.bits.eq_class_id := snoop_outer.io.req.bits.eq_class_id
  snoop_outer.io.req.ready := core.io.prefetch_req.ready

  val fetcher = Module(new DRKFetcher()(p))
  fetcher.io.config.key_table_base := mask_table_base

  core.io.fetch_req.ready := fetcher.io.req.ready
  fetcher.io.req.valid := core.io.fetch_req.valid & enable
  fetcher.io.req.bits.eq_class_idx := core.io.fetch_req.bits.eq_class_idx

  io.mem <> fetcher.io.tl

  fetcher.io.resp.ready := core.io.fetch_resp.ready
  core.io.fetch_resp.valid := fetcher.io.resp.valid
  core.io.fetch_resp.bits.data := fetcher.io.resp.bits.keys

}
