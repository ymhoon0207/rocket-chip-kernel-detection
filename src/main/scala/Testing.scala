// See LICENSE for license details.

package rocketchip

import Chisel._
import scala.collection.mutable.{LinkedHashSet,LinkedHashMap}
import cde.{Parameters, ParameterDump, Config}
import uncore.AllSCRFiles

abstract class RocketTestSuite {
  val dir: String
  val makeTargetName: String
  val names: LinkedHashSet[String]
  val envName: String
  def postScript = s"""

$$(addprefix $$(output_dir)/, $$(addsuffix .hex, $$($makeTargetName))): $$(output_dir)/%.hex: $dir/%.hex
\tmkdir -p $$(output_dir)
\tln -fs $$< $$@

$$(addprefix $$(output_dir)/, $$($makeTargetName)): $$(output_dir)/%: $dir/%
\tmkdir -p $$(output_dir)
\tln -fs $$< $$@

run-$makeTargetName: $$(addprefix $$(output_dir)/, $$(addsuffix .out, $$($makeTargetName)))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if /\\*{3}(.{8})\\*{3}(.*)/' $$^; echo;

run-$makeTargetName-debug: $$(addprefix $$(output_dir)/, $$(addsuffix .vpd, $$($makeTargetName)))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if /\\*{3}(.{8})\\*{3}(.*)/' $$(patsubst %.vpd,%.out,$$^); echo;
"""
}

trait GroundTestSuite extends RocketTestSuite {
  override def postScript = s"""

$$(addprefix $$(output_dir)/, $$(addsuffix .hex, $$($makeTargetName))):
\tmkdir -p $$(output_dir)
\ttouch $$@

$$(addprefix $$(output_dir)/, $$($makeTargetName)):
\tmkdir -p $$(output_dir)
\ttouch $$@

run-$makeTargetName: $$(addprefix $$(output_dir)/, $$(addsuffix .out, $$($makeTargetName)))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if /\\*{3}(.{8})\\*{3}(.*)/' $$^; echo;

run-$makeTargetName-debug: $$(addprefix $$(output_dir)/, $$(addsuffix .vpd, $$($makeTargetName)))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if /\\*{3}(.{8})\\*{3}(.*)/' $$(patsubst %.vpd,%.out,$$^); echo;
  """
}

class AssemblyTestSuite(makePrefix: String, toolsPrefix: String, val names: LinkedHashSet[String])(val envName: String) extends RocketTestSuite {
  val dir = "$(RISCV)/riscv64-unknown-elf/share/riscv-tests/isa"
  val makeTargetName = makePrefix + "-" + envName + "-asm-tests"
  override def toString = s"$makeTargetName = \\\n" + names.map(n => s"\t$toolsPrefix-$envName-$n").mkString(" \\\n") + postScript
}

class BenchmarkTestSuite(makePrefix: String, val dir: String, val names: LinkedHashSet[String]) extends RocketTestSuite {
  val envName = ""
  val makeTargetName = makePrefix + "-bmark-tests"
  override def toString = s"$makeTargetName = \\\n" + names.map(n => s"\t$n.riscv").mkString(" \\\n") + postScript
}

class AssemblyGroundTestSuite extends AssemblyTestSuite("","",LinkedHashSet())("") with GroundTestSuite {
  override val dir = ""
  override val names = LinkedHashSet[String]()
  override val makeTargetName = "unit-test"
  override def toString = s"$makeTargetName = unit-test\\\n" + postScript
}

class BenchmarkGroundTestSuite extends BenchmarkTestSuite("", "", LinkedHashSet()) with GroundTestSuite {
  override val makeTargetName = "unit-bmark-tests"
  override def toString = s"$makeTargetName = unit-test\\\n" + postScript
}

object TestGeneration extends FileSystemUtilities{
  import scala.collection.mutable.HashMap
  val asmSuites = new LinkedHashMap[String,AssemblyTestSuite]()
  val bmarkSuites = new  HashMap[String,BenchmarkTestSuite]()

  def addSuite(s: RocketTestSuite) {
    s match {
      case a: AssemblyTestSuite => asmSuites += (a.makeTargetName -> a)
      case b: BenchmarkTestSuite => bmarkSuites += (b.makeTargetName -> b)
    }
  }
  
  def addSuites(s: Seq[RocketTestSuite]) { s.foreach(addSuite) }

  def generateMakefrag(topModuleName: String, configClassName: String) {
    def gen(kind: String, s: Seq[RocketTestSuite]) = {
      if(s.length > 0) {
        val envs = s.groupBy(_.envName)
        val targets = s.map(t => s"$$(${t.makeTargetName})").mkString(" ")
        s.map(_.toString).mkString("\n") +
        envs.filterKeys(_ != "").map( {
          case (env,envsuites) => {
          val suites = envsuites.map(t => s"$$(${t.makeTargetName})").mkString(" ")
        s"""
run-$kind-$env-tests: $$(addprefix $$(output_dir)/, $$(addsuffix .out, $suites))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if /\\*{3}(.{8})\\*{3}(.*)/' $$^; echo;
run-$kind-$env-tests-debug: $$(addprefix $$(output_dir)/, $$(addsuffix .vpd, $suites))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if /\\*{3}(.{8})\\*{3}(.*)/' $$(patsubst %.vpd,%.out,$$^); echo;
run-$kind-$env-tests-fast: $$(addprefix $$(output_dir)/, $$(addsuffix .run, $suites))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if /\\*{3}(.{8})\\*{3}(.*)/' $$^; echo;
"""} } ).mkString("\n") + s"""
run-$kind-tests: $$(addprefix $$(output_dir)/, $$(addsuffix .out, $targets))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if /\\*{3}(.{8})\\*{3}(.*)/' $$^; echo;
run-$kind-tests-debug: $$(addprefix $$(output_dir)/, $$(addsuffix .vpd, $targets))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if /\\*{3}(.{8})\\*{3}(.*)/' $$(patsubst %.vpd,%.out,$$^); echo;
run-$kind-tests-fast: $$(addprefix $$(output_dir)/, $$(addsuffix .run, $targets))
\t@echo; perl -ne 'print "  [$$$$1] $$$$ARGV \\t$$$$2\\n" if /\\*{3}(.{8})\\*{3}(.*)/' $$^; echo;
"""
      } else { "\n" }
    }

    val f = createOutputFile(s"$topModuleName.$configClassName.d")
    f.write(
      List(
        gen("asm", asmSuites.values.toSeq),
        gen("bmark", bmarkSuites.values.toSeq)
      ).mkString("\n"))
    f.close
  }
}

object DefaultTestSuites {
  val rv32uiNames = LinkedHashSet(
    "simple", "add", "addi", "and", "andi", "auipc", "beq", "bge", "bgeu", "blt", "bltu", "bne", "fence_i", 
    "j", "jal", "jalr", "lb", "lbu", "lh", "lhu", "lui", "lw", "or", "ori", "sb", "sh", "sw", "sll", "slli",
    "slt", "slti", "sra", "srai", "srl", "srli", "sub", "xor", "xori")
  val rv32ui = new AssemblyTestSuite("rv32ui", "rv32ui", rv32uiNames)(_)

  val rv32umNames = LinkedHashSet("mul", "mulh", "mulhsu", "mulhu", "div", "divu", "rem", "remu")
  val rv32um = new AssemblyTestSuite("rv32um", "rv32ui", rv32umNames)(_)

  val rv32uaNames = LinkedHashSet("amoadd_w", "amoand_w", "amoor_w", "amoxor_w", "amoswap_w", "amomax_w", "amomaxu_w", "amomin_w", "amominu_w")
  val rv32ua = new AssemblyTestSuite("rv32ua", "rv32ui", rv32uaNames)(_)

  val rv32siNames = LinkedHashSet("csr", "ma_fetch", "scall", "sbreak", "wfi")
  val rv32si = new AssemblyTestSuite("rv32si", "rv32si", rv32siNames)(_)

  val rv32miNames = LinkedHashSet("csr", "mcsr", "wfi", "dirty", "illegal", "ma_addr", "ma_fetch", "sbreak", "scall", "timer")
  val rv32mi = new AssemblyTestSuite("rv32mi", "rv32mi", rv32miNames)(_)

  val rv32u = List(rv32ui, rv32um, rv32ua)
  val rv32i = List(rv32ui, rv32si, rv32mi)

  val rv64uiNames = LinkedHashSet("addw", "addiw", "ld", "lwu", "sd", "slliw", "sllw", "sltiu", "sltu", "sraiw", "sraw", "srliw", "srlw", "subw")
  val rv64ui = new AssemblyTestSuite("rv64ui", "rv64ui", rv32uiNames ++ rv64uiNames)(_)

  val rv64umNames = LinkedHashSet("divuw", "divw", "mulw", "remuw", "remw")
  val rv64um = new AssemblyTestSuite("rv64um", "rv64ui", rv32umNames ++ rv64umNames)(_)

  val rv64uaNames = rv32uaNames.map(_.replaceAll("_w","_d"))
  val rv64ua = new AssemblyTestSuite("rv64ua", "rv64ui", rv32uaNames ++ rv64uaNames)(_)

  val rv64ufNames = LinkedHashSet("ldst", "move", "fsgnj", "fcmp", "fcvt", "fcvt_w", "fclass", "fadd", "fdiv", "fmin", "fmadd", "structural")
  val rv64uf = new AssemblyTestSuite("rv64uf", "rv64uf", rv64ufNames)(_)
  val rv64ufNoDiv = new AssemblyTestSuite("rv64uf", "rv64uf", rv64ufNames - "fdiv")(_)

  val rv64siNames = rv32siNames
  val rv64si = new AssemblyTestSuite("rv64si", "rv64si", rv64siNames)(_)

  val rv64miNames = rv32miNames
  val rv64mi = new AssemblyTestSuite("rv64mi", "rv64mi", rv64miNames)(_)

  // TODO: "rv64ui-pm-lrsc", "rv64mi-pm-ipi",

  val rv64u = List(rv64ui, rv64um, rv64ua)
  val rv64i = List(rv64ui, rv64si, rv64mi)

  val bmarks = new BenchmarkTestSuite("basic", "$(RISCV)/riscv64-unknown-elf/share/riscv-tests/benchmarks", LinkedHashSet(
    "median", "multiply", "qsort", "towers", "vvadd", "mm", "dhrystone", "spmv", "mt-vvadd", "mt-matmul", "test-dr"))

  val mtBmarks = new BenchmarkTestSuite("mt", "$(RISCV)/riscv64-unknown-elf/share/riscv-tests/mt",
    LinkedHashSet(((0 to 4).map("vvadd"+_) ++
    List("ad","ae","af","ag","ai","ak","al","am","an","ap","aq","ar","at","av","ay","az",
         "bb","bc","bf","bh","bj","bk","bm","bo","br","bs","ce","cf","cg","ci","ck","cl",
         "cm","cs","cv","cy","dc","df","dm","do","dr","ds","du","dv").map(_+"_matmul")): _*))

  val zscaleBmarks = new BenchmarkTestSuite("zscale", "$(base_dir)/zscale/sw", LinkedHashSet(
    "led", "mbist"))
}

object TestGenerator extends App with FileSystemUtilities {
  val projectName = args(0)
  val topModuleName = args(1)
  val configClassName = args(2)
  val config = try {
      Class.forName(s"$projectName.$configClassName").newInstance.asInstanceOf[Config]
    } catch {
      case e: java.lang.ClassNotFoundException =>
        throwException("Unable to find configClassName \"" + configClassName +
                       "\", did you misspell it?", e)
    }
  val world = config.toInstance
  val paramsFromConfig: Parameters = Parameters.root(world)

  val gen = () => 
    Class.forName(s"$projectName.$topModuleName")
      .getConstructor(classOf[cde.Parameters])
      .newInstance(paramsFromConfig)
      .asInstanceOf[Module]

  chiselMain.run(args.drop(3), gen)
  //Driver.elaborate(gen, configName = configClassName)

  TestGeneration.generateMakefrag(topModuleName, configClassName)
  TestBenchGeneration.generateVerilogFragment(
    topModuleName, configClassName,
    paramsFromConfig(NMemoryChannels))
  TestBenchGeneration.generateCPPFragment(
    topModuleName, configClassName,
    paramsFromConfig(NMemoryChannels))

  val pdFile = createOutputFile(s"$topModuleName.$configClassName.prm")
  pdFile.write(ParameterDump.getDump)
  pdFile.close
  val v = createOutputFile(configClassName + ".knb")
  v.write(world.getKnobs)
  v.close
  val d = new java.io.FileOutputStream(Driver.targetDir + "/" + configClassName + ".dtb")
  d.write(paramsFromConfig(DeviceTree))
  d.close
  val w = createOutputFile(configClassName + ".cst")
  w.write(world.getConstraints)
  w.close
  val scr_map_hdr = createOutputFile(topModuleName + "." + configClassName + ".scr_map.h")
  AllSCRFiles.foreach{ map => scr_map_hdr.write(map.as_c_header) }
  scr_map_hdr.close
}
