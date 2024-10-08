// Finitie impulse filter
package f2_universal
import config._
import config.{f2Config}

import java.io.File

import chisel3._
import chisel3.util.{switch, is}

import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.stage.ChiselGeneratorAnnotation

import dsptools._
import dsptools.numbers.DspComplex

import hb_universal._
import cic_universal._

class f2_universalCLK extends Bundle {
    val pn   = Input(Clock())
    val p2n    = Input(Clock())
    val p4n    = Input(Clock())
    val p8n    = Input(Clock())
}

class f2_universalCTRL(val resolution : Int, val gainBits: Int) extends Bundle {
    val cic3scale = Input(UInt(gainBits.W))
    val reset_loop = Input(Bool())
    val reset_clk = Input(Bool())
    val hb1scale = Input(UInt(gainBits.W))
    val hb1output_switch = Input(UInt(1.W))
    val hb2scale = Input(UInt(gainBits.W))
    val hb2output_switch = Input(UInt(1.W))
    val hb3scale = Input(UInt(gainBits.W))
    val hb3output_switch = Input(UInt(1.W))
    val mode = Input(UInt(3.W))
    val convmode = Input(UInt(1.W))
    val in_valid = Input(UInt(1.W))
    val out_valid = Output(UInt(1.W))
}

class f2_universalIO(resolution: Int, gainBits: Int) extends Bundle {
    val clock = new f2_universalCLK    
    val control = new f2_universalCTRL(resolution=resolution,gainBits=gainBits)
    val in = new Bundle {
        val iptr_A = Input(DspComplex(SInt(resolution.W), SInt(resolution.W)))
    }
    val out = new Bundle {
        val Z = Output(DspComplex(SInt(resolution.W), SInt(resolution.W)))
    }
}

class f2_universal(config: f2Config) extends Module {
    val io = IO(new f2_universalIO(resolution=config.resolution, gainBits=config.gainBits))
    val data_reso = config.resolution
    val calc_reso = config.resolution * 2
    
    val state = RegInit(5.U(3.W))

    state :=  io.control.mode

    //Reset initializations
    val hb1reset = Wire(Bool())
    val hb2reset = Wire(Bool())
    val hb3reset = Wire(Bool())
    val cic3reset = Wire(Bool())
    hb1reset := reset.asBool
    hb2reset := reset.asBool
    hb3reset := reset.asBool
    cic3reset := reset.asBool
    
    val hb1 = withClockAndReset(io.clock.p4n, hb1reset)(Module( 
        new hb_universal(config=config.hb1_config)
    ))

    val hb2 = withClockAndReset(io.clock.p2n, hb2reset)(Module( 
        new hb_universal(config=config.hb2_config)
    ))

    val hb3 = withClockAndReset(io.clock.pn, hb3reset)(Module(
        new hb_universal(config=config.hb3_config)
    ))

    val cic3 = withClockAndReset(clock, cic3reset)(Module(
        new cic_universal(config=config.cic3_config)
    ))

    hb1.io.clock.slow  := io.clock.p8n
    hb2.io.clock.slow  := io.clock.p4n
    hb3.io.clock.slow  := io.clock.p2n
    cic3.io.clock.slow  := io.clock.pn

    hb1.io.control.convmode     := io.control.convmode
    hb2.io.control.convmode     := io.control.convmode
    hb3.io.control.convmode     := io.control.convmode
    cic3.io.control.convmode    := io.control.convmode

    hb1.io.control.scale        := io.control.hb1scale
    hb2.io.control.scale        := io.control.hb2scale
    hb3.io.control.scale        := io.control.hb3scale    
    cic3.io.control.scale       := io.control.cic3scale

    hb1.io.control.output_switch  := io.control.hb1output_switch
    hb2.io.control.output_switch  := io.control.hb2output_switch
    hb3.io.control.output_switch  := io.control.hb3output_switch
    cic3.io.control.reset_loop    := io.control.reset_loop

    //Default is to bypass
    hb1.io.in.iptr_A    := cic3.io.out.Z
    hb2.io.in.iptr_A    := hb1.io.out.Z
    hb3.io.in.iptr_A    := hb2.io.out.Z
    cic3.io.in.iptr_A   := io.in.iptr_A

    io.out.Z            := RegNext(io.in.iptr_A) 

    hb1.io.control.in_valid    := io.control.in_valid
    hb2.io.control.in_valid    := hb1.io.control.out_valid 
    hb3.io.control.in_valid    := hb2.io.control.out_valid 
    cic3.io.control.in_valid   := hb3.io.control.out_valid 
    io.control.out_valid       := cic3.io.control.out_valid 
   
    //Decoder for the modes
    when(io.control.convmode === 1.U){
        when(state === 1.U) { //Two
            cic3reset           := true.B 
            hb1reset            := reset.asBool
            hb2reset            := true.B
            hb3reset            := true.B

            hb1.io.in.iptr_A    := io.in.iptr_A
            io.out.Z            := hb1.io.out.Z
            hb1.io.control.in_valid    := io.control.in_valid
            io.control.out_valid       := hb1.io.control.out_valid  
        } .elsewhen(state === 2.U) { //Four
            cic3reset           := true.B 
            hb1reset            := reset.asBool
            hb2reset            := reset.asBool
            hb3reset            := true.B

            hb2.io.in.iptr_A    := io.in.iptr_A
            hb1.io.in.iptr_A    := hb2.io.out.Z
            io.out.Z            := hb1.io.out.Z
            hb2.io.control.in_valid    := io.control.in_valid
            hb1.io.control.in_valid    := hb2.io.control.out_valid 
            io.control.out_valid       := hb1.io.control.out_valid
        } .elsewhen(state === 3.U) { //Eight
            cic3reset           := true.B
            hb1reset            := reset.asBool 
            hb2reset            := reset.asBool
            hb3reset            := reset.asBool

            hb3.io.in.iptr_A    := io.in.iptr_A
            hb2.io.in.iptr_A    := hb3.io.out.Z
            hb1.io.in.iptr_A    := hb2.io.out.Z
            io.out.Z            := hb1.io.out.Z
            hb3.io.control.in_valid    := io.control.in_valid
            hb2.io.control.in_valid    := hb3.io.control.out_valid 
            hb1.io.control.in_valid    := hb2.io.control.out_valid 
            io.control.out_valid       := hb1.io.control.out_valid 
        } .elsewhen(state === 4.U) { //More
            cic3reset           := reset.asBool
            hb1reset            := reset.asBool
            hb2reset            := reset.asBool
            hb3reset            := reset.asBool

            cic3.io.in.iptr_A   := io.in.iptr_A
            hb3.io.in.iptr_A    := cic3.io.out.Z
            hb2.io.in.iptr_A    := hb3.io.out.Z
            hb1.io.in.iptr_A    := hb2.io.out.Z
            io.out.Z            := hb1.io.out.Z
            cic3.io.control.in_valid   := io.control.in_valid
            hb3.io.control.in_valid    := cic3.io.control.out_valid 
            hb2.io.control.in_valid    := hb3.io.control.out_valid 
            hb1.io.control.in_valid    := hb2.io.control.out_valid 
            io.control.out_valid       := hb1.io.control.out_valid 
        } .elsewhen(state === 5.U) { //Init
            cic3reset           := reset.asBool
            hb1reset            := reset.asBool
            hb2reset            := reset.asBool
            hb3reset            := reset.asBool

            cic3.io.in.iptr_A   := io.in.iptr_A
            hb3.io.in.iptr_A    := cic3.io.out.Z
            hb2.io.in.iptr_A    := hb3.io.out.Z
            hb1.io.in.iptr_A    := hb2.io.out.Z
            io.out.Z            := RegNext(io.in.iptr_A)
            cic3.io.control.in_valid   := io.control.in_valid
            hb3.io.control.in_valid    := cic3.io.control.out_valid 
            hb2.io.control.in_valid    := hb3.io.control.out_valid 
            hb1.io.control.in_valid    := hb2.io.control.out_valid 
            io.control.out_valid       := hb1.io.control.out_valid 
        }.otherwise { //Bypass
            cic3reset           := true.B 
            hb1reset            := true.B
            hb2reset            := true.B
            hb3reset            := true.B

            io.out.Z            := RegNext(io.in.iptr_A)
            io.control.out_valid   := io.control.in_valid
        }
      }.otherwise {
        when(state === 1.U) { //Two
            cic3reset           := true.B 
            hb1reset            := reset.asBool
            hb2reset            := true.B
            hb3reset            := true.B

            hb1.io.in.iptr_A    := io.in.iptr_A
            io.out.Z            := hb1.io.out.Z
            hb1.io.control.in_valid    := io.control.in_valid
            io.control.out_valid       := hb1.io.control.out_valid 
        } .elsewhen(state === 2.U) { //Four
            cic3reset           := true.B 
            hb1reset            := reset.asBool
            hb2reset            := reset.asBool
            hb3reset            := true.B

            hb1.io.in.iptr_A    := io.in.iptr_A
            hb2.io.in.iptr_A    := hb1.io.out.Z
            io.out.Z            := hb2.io.out.Z
            hb1.io.control.in_valid    := io.control.in_valid
            hb2.io.control.in_valid    := hb1.io.control.out_valid 
            io.control.out_valid       := hb2.io.control.out_valid  
        } .elsewhen(state === 3.U) { //Eight
            cic3reset           := true.B
            hb1reset            := reset.asBool 
            hb2reset            := reset.asBool
            hb3reset            := reset.asBool

            hb1.io.in.iptr_A    := io.in.iptr_A
            hb2.io.in.iptr_A    := hb1.io.out.Z
            hb3.io.in.iptr_A    := hb2.io.out.Z
            io.out.Z            := hb3.io.out.Z
            hb1.io.control.in_valid    := io.control.in_valid
            hb2.io.control.in_valid    := hb1.io.control.out_valid 
            hb3.io.control.in_valid    := hb2.io.control.out_valid 
            io.control.out_valid       := hb3.io.control.out_valid
        } .elsewhen(state === 4.U) { //More
            cic3reset           := reset.asBool
            hb1reset            := reset.asBool
            hb2reset            := reset.asBool
            hb3reset            := reset.asBool

            hb1.io.in.iptr_A    := io.in.iptr_A
            hb2.io.in.iptr_A    := hb1.io.out.Z
            hb3.io.in.iptr_A    := hb2.io.out.Z
            cic3.io.in.iptr_A   := hb3.io.out.Z
            io.out.Z            := cic3.io.out.Z
            hb1.io.control.in_valid    := io.control.in_valid
            hb2.io.control.in_valid    := hb1.io.control.out_valid 
            hb3.io.control.in_valid    := hb2.io.control.out_valid 
            cic3.io.control.in_valid   := hb3.io.control.out_valid 
            io.control.out_valid       := cic3.io.control.out_valid 
        } .elsewhen(state === 5.U) { //Init
            cic3reset           := reset.asBool
            hb1reset            := reset.asBool
            hb2reset            := reset.asBool
            hb3reset            := reset.asBool

            hb1.io.in.iptr_A    := io.in.iptr_A
            hb2.io.in.iptr_A    := hb1.io.out.Z
            hb3.io.in.iptr_A    := hb2.io.out.Z
            cic3.io.in.iptr_A   := hb3.io.out.Z
            io.out.Z            := RegNext(io.in.iptr_A)
            hb1.io.control.in_valid    := io.control.in_valid
            hb2.io.control.in_valid    := hb1.io.control.out_valid 
            hb3.io.control.in_valid    := hb2.io.control.out_valid 
            cic3.io.control.in_valid   := hb3.io.control.out_valid 
            io.control.out_valid       := cic3.io.control.out_valid 
        } .otherwise { //Bypass
            cic3reset           := true.B 
            hb1reset            := true.B
            hb2reset            := true.B
            hb3reset            := true.B

            io.out.Z               := RegNext(io.in.iptr_A)
            io.control.out_valid   := io.control.in_valid
        }

      }
}



/** Generates verilog or sv*/
object f2_universal extends App with OptionParser {
    // Parse command-line arguments
    val (options, arguments) = getopts(default_opts, args.toList)
    printopts(options, arguments)

    val f2config_file = options("f2config_file")
    val hb1config_file = options("hb1config_file")
    val hb2config_file = options("hb2config_file")
    val hb3config_file = options("hb3config_file")
    val cic3config_file = options("cic3config_file")
    val target_dir = options("td")

    var f2_config: Option[f2Config] = None

    f2Config.loadFromFile(
        f2config_file, 
        hb1config_file, 
        hb2config_file, 
        hb3config_file, 
        cic3config_file) match {
        case Left(config) => {
            f2_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load f2 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    // Generate verilog
    val annos = Seq(ChiselGeneratorAnnotation(() => new f2_universal(config=f2_config.get)))
    val sysverilog = (new ChiselStage).emitSystemVerilog(
        new f2_universal(config=f2_config.get),
     
    //args
    Array("--target-dir", target_dir))
}



/** Module-specific command-line option parser */
trait OptionParser {
  // Module specific command-line option flags
  val available_opts: List[String] = List(
      "-f2config_file",
      "-hb1config_file",
      "-hb2config_file",
      "-hb3config_file",
      "-cic3config_file",
      "-td"
  )

  // Default values for the command-line options
  val default_opts : Map[String, String] = Map(
    "f2config_file"->"f2-config.yml",
    "hb1config_file"->"hb_universal/configs/hb1-config.yml",
    "hb2config_file"->"hb_universal/configs/hb2-config.yml",
    "hb3config_file"->"hb_universal/configs/hb3-config.yml",
    "cic3config_file"->"cic_universal/configs/cic3-config.yml",
    "td"->"verilog/"
  )

  /** Recursively parse option flags from command line args
   * @param options Map of command line option names to their respective values.
   * @param arguments List of arguments to parse.
   * @return a tuple whose first element is the map of parsed options to their values 
   *         and the second element is the list of arguments that don't take any values.
   */
  def getopts(options: Map[String, String], arguments: List[String]) : (Map[String, String], List[String]) = {
    val usage = s"""
      |Usage: ${this.getClass.getName.replace("$","")} [-<option> <argument>]
      |
      | Options
      |     -f2config_file      [String]  : Generator YAML configuration file name. Default "f2-config.yml".
      |     -hb1config_file     [String]  : Generator YAML configuration file name. Default "hb1-config.yml".
      |     -hb2config_file     [String]  : Generator YAML configuration file name. Default "hb2-config.yml".
      |     -hb3config_file     [String]  : Generator YAML configuration file name. Default "hb3-config.yml".
      |     -cic3config_file    [String]  : Generator YAML configuration file name. Default "cic3-config.yml".
      |     -td                 [String]  : Target dir for building. Default "verilog/".
      |     -h                            : Show this help message.
      """.stripMargin

    // Parse next elements in argument list
    arguments match {
      case "-h" :: tail => {
        println(usage)
        sys.exit()
      }
      case option :: value :: tail if available_opts contains option => {
        val (newopts, newargs) = getopts(
            options ++ Map(option.replace("-","") -> value), tail
        )
        (newopts, newargs)
      }
      case argument :: tail => {
        val (newopts, newargs) = getopts(options, tail)
        (newopts, argument.toString +: newargs)
      }
      case Nil => (options, arguments)
    }
  }

  /** Print parsed options and arguments to stdout */
  def printopts(options: Map[String, String], arguments: List[String]) = {
    println("\nCommand line options:")
    options.nonEmpty match {
      case true => for ((k,v) <- options) {
        println(s"  $k = $v")
      }
      case _ => println("  None")
    }
    println("\nCommand line arguments:")
    arguments.nonEmpty match {
      case true => for (arg <- arguments) {
        println(s"  $arg")
      }
      case _ => println("  None")
    }
  }
}

