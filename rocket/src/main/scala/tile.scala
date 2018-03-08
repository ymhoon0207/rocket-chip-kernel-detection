// See LICENSE for license details.

package rocket

import Chisel._
import uncore._
import Util._
import cde.{Parameters, Field}

case object CoreName extends Field[String]
case object BuildRoCC extends Field[Seq[RoccParameters]]

case class RoccParameters(
  opcodes: OpcodeSet,
  generator: Parameters => RoCC,
  nMemChannels: Int = 0,
  nPTWPorts : Int = 0,
  csrs: Seq[Int] = Nil,
  useFPU: Boolean = false)

abstract class Tile(resetSignal: Bool = null)
                   (implicit p: Parameters) extends Module(_reset = resetSignal) {
  val buildRocc = p(BuildRoCC)
  val usingRocc = !buildRocc.isEmpty
  val nRocc = buildRocc.size
  val nFPUPorts = buildRocc.filter(_.useFPU).size
  val nDCachePorts = 2 + nRocc
  val nPTWPorts = 2 + p(RoccNPTWPorts)
  val nCachedTileLinkPorts = 1
  val nUncachedTileLinkPorts = 1 + p(RoccNMemChannels)
  val dcacheParams = p.alterPartial({ case CacheName => "L1D" })
  val io = new Bundle {
    val cached = Vec(nCachedTileLinkPorts, new ClientTileLinkIO)
    val uncached = Vec(nUncachedTileLinkPorts, new ClientUncachedTileLinkIO)
    val host = new HtifIO
    val dma = new DmaIO
    val drk_config = Vec(16,UInt(INPUT))
    val drk_pc = Vec(32,UInt(OUTPUT))
  }
}

class RocketTile(resetSignal: Bool = null)(implicit p: Parameters) extends Tile(resetSignal)(p) {
  val core = Module(new Rocket()(p.alterPartial({ case CoreName => "Rocket" })))
  val icache = Module(new Frontend()(p.alterPartial({
    case CacheName => "L1I"
    case CoreName => "Rocket" })))
  val dcache = Module(new HellaCache()(dcacheParams))
  val ptw = Module(new PTW(nPTWPorts)(dcacheParams))
  val kcache = Module(new DRKCache()(p.alterPartial({
  					case CacheName => "L1I"
					case CoreName => "Rocket"
					})))
  val kstack = Module(new DRKStackTop()(p.alterPartial({
  					case CacheName => "L1I"
					case CoreName => "Rocket"
					})))

  val dr_pc_icache_miss = Reg(init=UInt(0,width=64))
  val dr_pc_dcache_miss = Reg(init=UInt(0,width=64))
  val dr_pc_kcache_miss = Reg(init=UInt(0,width=64))
  val dr_pc_kstack_wb = Reg(init=UInt(0,width=64))
  val dr_pc_kstack_fetch = Reg(init=UInt(0,width=64))
  val dr_pc_dcache_wb = Reg(init=UInt(0,width=64))
  val dr_pc_max_stack_len = Reg(init=UInt(0,width=64))
  val dr_pc_kstack_wb_internal = Reg(init=UInt(0,width=64))
  val dr_pc_kstack_fetch_internal = Reg(init=UInt(0,width=64))

  core.io.status_stack := kstack.io.status_core

  when(kcache.io.reset_perf_counters === UInt(0)){
    when(dcache.io.mem.acquire.fire() && dcache.io.mem.acquire.bits.hasData() ||
	 dcache.io.mem.release.fire() && dcache.io.mem.release.bits.hasData())
       {
      dr_pc_dcache_wb := dr_pc_dcache_wb + UInt(1)
      printf("dcache_wb:\t%d\n",dr_pc_dcache_wb + UInt(1))
    }
    when(dcache.io.mem.grant.fire()){
       when(dcache.io.mem.grant.bits.hasData()){
        dr_pc_dcache_miss := dr_pc_dcache_miss + UInt(1)
        printf("dcache_miss:\t%d\n",dr_pc_dcache_miss + UInt(1))
      } 
    }
    when(icache.io.mem.acquire.fire()){ 
      dr_pc_icache_miss := dr_pc_icache_miss + UInt(1)
      printf("icache_miss:\t%d\n",dr_pc_icache_miss + UInt(1))
    }
    when(kcache.io.mem.acquire.fire()){
      dr_pc_kcache_miss := dr_pc_kcache_miss + UInt(1)
      printf("kcache_miss:\t%d\n",dr_pc_kcache_miss + UInt(1))
    }
    when(kstack.io.mem.grant.fire()){
      when(kstack.io.mem.acquire.bits.a_type === Acquire.getBlockType){
        dr_pc_kstack_fetch := dr_pc_kstack_fetch + UInt(1)
      } 
    }
    when(kstack.io.mem.acquire.fire()){
      when(kstack.io.mem.acquire.bits.hasData()){
        dr_pc_kstack_wb := dr_pc_kstack_wb + UInt(1)
      } 
    }
    when(kstack.io.status.active === Bool(true)){

      when(kstack.io.perf_stack_top > dr_pc_max_stack_len){
        dr_pc_max_stack_len := kstack.io.perf_stack_top
      }
      when(kstack.io.perf_wb_internal){
        dr_pc_kstack_wb_internal := dr_pc_kstack_wb_internal + UInt(1)
      }
      when(kstack.io.perf_fetch_internal){
        dr_pc_kstack_fetch_internal := dr_pc_kstack_fetch_internal + UInt(1)
      }
    }.otherwise{
      dr_pc_max_stack_len := UInt(0)
      dr_pc_kstack_wb_internal := UInt(0)
      dr_pc_kstack_fetch_internal := UInt(0)
    }
    
  }.elsewhen(kcache.io.reset_perf_counters === UInt(1)){
    dr_pc_icache_miss := UInt(0)
    dr_pc_dcache_miss := UInt(0)
    dr_pc_kcache_miss := UInt(0)
    dr_pc_kstack_wb := UInt(0)
    dr_pc_kstack_fetch := UInt(0)
    dr_pc_dcache_wb := UInt(0)
    dr_pc_max_stack_len := UInt(0)
    dr_pc_kstack_wb_internal := UInt(0)
    dr_pc_kstack_wb_internal := UInt(0)


  }.otherwise{
    dr_pc_icache_miss := dr_pc_icache_miss
    dr_pc_dcache_miss := dr_pc_dcache_miss
    dr_pc_kcache_miss := dr_pc_kcache_miss
    dr_pc_kstack_wb := dr_pc_kstack_wb
    dr_pc_kstack_fetch := dr_pc_kstack_fetch
    dr_pc_dcache_wb := dr_pc_dcache_wb
    dr_pc_max_stack_len := dr_pc_max_stack_len
    dr_pc_kstack_wb_internal := dr_pc_kstack_wb_internal
    dr_pc_kstack_fetch_internal := dr_pc_kstack_fetch_internal
  }

  io.drk_pc(0) := dr_pc_icache_miss
  io.drk_pc(1) := dr_pc_dcache_miss
  io.drk_pc(2) := dr_pc_dcache_wb
  io.drk_pc(3) := dr_pc_kcache_miss
  io.drk_pc(4) := dr_pc_kstack_fetch
  io.drk_pc(5) := dr_pc_kstack_wb
  io.drk_pc(6) := dr_pc_max_stack_len
  io.drk_pc(7) := dr_pc_kstack_fetch_internal
  io.drk_pc(8) := dr_pc_kstack_wb_internal

  for(i <- 9 until 32){
    io.drk_pc(i) := UInt(0xdead,width=64)
  }

  kstack.io.rf_req <> core.io.drkstack_rf_req
  kstack.io.rf_resp <> core.io.drkstack_rf_resp

  kcache.io.fe_next_insn := icache.io.dr_next_insn

  core.io.kcache_fast_path_in := kcache.io.cpu_fast_path_out
  kcache.io.cpu_fast_path_in := core.io.kcache_fast_path_out

  core.io.static_table_base_in := kcache.io.static_table_base_out
  kcache.io.static_table_base_in := core.io.static_table_base_out

  kcache.io.cpu <> core.io.dr_mask
  core.io.dr_enable := kcache.io.enable
  core.io.drkstack <> kstack.io.cmd
  kstack.io.config := core.io.dr_config
  kcache.io.active := kstack.io.status.active
  core.io.dr_status := kstack.io.status

  kcache.io.s_grant.bits := icache.io.mem.grant.bits
  kcache.io.s_grant.valid := icache.io.mem.grant.fire()
  kcache.io.s_resp.bits := icache.io.cpu.resp.bits
  kcache.io.s_resp.valid := icache.io.cpu.resp.valid

  kcache.io.config := io.drk_config 
 
  dcache.io.cpu.invalidate_lr := core.io.dmem.invalidate_lr // Bypass signal to dcache
  val dcArb = Module(new HellaCacheArbiter(nDCachePorts)(dcacheParams))
  dcArb.io.requestor(0) <> ptw.io.mem
  dcArb.io.requestor(1) <> core.io.dmem
/*
  dcArb.io.requestor(1).bits := core.io.dmem.bits
  dcArb.io.requestor(1).valid := core.io.dmem.valid & kcache.io.cpu.valid
  core.io.dmem.ready := dcArb.io.requestor(1).ready & kcache.io.cpu.valid
  kcache.io.cpu.ready := core.io.dmem.fire()
*/
  dcache.io.cpu <> dcArb.io.mem

  ptw.io.requestor(0) <> icache.io.ptw
  ptw.io.requestor(1) <> dcache.io.ptw

  io.host <> core.io.host
  icache.io.cpu <> core.io.imem
  core.io.ptw <> ptw.io.dpath

  val fpuOpt = if (p(UseFPU)) Some(Module(new FPU)) else None
  fpuOpt.foreach(fpu => core.io.fpu <> fpu.io)

   // Connect the caches and ROCC to the outer memory system
  io.cached.head <> dcache.io.mem
  // If so specified, build an RoCC module and wire it to core + TileLink ports,
  // otherwise just hookup the icache

   val uncachedArb = Module(new ClientTileLinkIOArbiter(3))
    uncachedArb.io.in(0) <> icache.io.mem 
    uncachedArb.io.in(1) <> kcache.io.mem
    uncachedArb.io.in(2) <> kstack.io.mem
  io.uncached <>  Seq(uncachedArb.io.out)
  
if (!usingRocc || nFPUPorts == 0) {
    fpuOpt.foreach { fpu =>
      fpu.io.cp_req.valid := Bool(false)
      fpu.io.cp_resp.ready := Bool(false)
    }
  }
}
