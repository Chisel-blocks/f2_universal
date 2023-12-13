// Finitie impulse filter
package f2_decimator
import config._
import config.{F2Config}

import java.io.File

import chisel3._
import chisel3.util.{log2Ceil, switch, is}
import chisel3.experimental.FixedPoint
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.stage.ChiselGeneratorAnnotation

import dsptools._
import dsptools.numbers.DspComplex

import hb_decimator._
import cic_decimator._

object F2States {
    object State extends ChiselEnum {
        val bypass, two, four, eight, more = Value
    }  
}

class F2_DecimatorCLK extends Bundle {
    val cic3clockslow   = Input(Clock())
    val hb1clock_low    = Input(Clock())
    val hb2clock_low    = Input(Clock())
    val hb3clock_low    = Input(Clock())
}

class F2_DecimatorCTRL(val resolution : Int, val gainBits: Int) extends Bundle {
    val cic3integscale = Input(UInt(gainBits.W))
    val cic3integshift = Input(UInt(log2Ceil(resolution).W))
    val reset_loop = Input(Bool())
    val hb1scale = Input(UInt(gainBits.W))
    val hb2scale = Input(UInt(gainBits.W))
    val hb3scale = Input(UInt(gainBits.W))
    val mode = Input(UInt(3.W))
}

class F2_DecimatorIO(resolution: Int, gainBits: Int) extends Bundle {
    val clock = new F2_DecimatorCLK    
    val control = new F2_DecimatorCTRL(resolution=resolution,gainBits=gainBits)
    val in = new Bundle {
        val iptr_A = Input(DspComplex(SInt(resolution.W), SInt(resolution.W)))
    }
    val out = new Bundle {
        val Z = Output(DspComplex(SInt(resolution.W), SInt(resolution.W)))
    }
}

class F2_Decimator(config: F2Config) extends Module {
    val io = IO(new F2_DecimatorIO(resolution=config.resolution, gainBits=config.gainBits))
    val data_reso = config.resolution
    val calc_reso = config.resolution * 2

    import F2States.State
    import F2States.State._

    //Select state with the fastest master clock
    val state = RegInit(bypass)
    
    //Decoder for the modes
    when(io.control.mode === 0.U){
        state := bypass
    } .elsewhen(io.control.mode === 1.U) {
        state := two
    } .elsewhen(io.control.mode === 2.U) {
        state := four
    } .elsewhen(io.control.mode === 3.U) {
        state := eight
    } .elsewhen(io.control.mode === 4.U) {
        state := more
    }.otherwise {
        state := bypass
    }
    
    //Reset initializations
    val hb1reset = Wire(Bool())
    val hb2reset = Wire(Bool())
    val hb3reset = Wire(Bool())
    hb1reset := reset.asBool
    hb2reset := reset.asBool
    hb3reset := reset.asBool

    val cic3reset = Wire(Bool())
    cic3reset := io.control.reset_loop
    
    val hb1 = withClockAndReset(io.clock.hb1clock_low, hb1reset)(Module( 
        new HB_Decimator(config=config.hb1_config)
    ))

    val hb2 = withClockAndReset(io.clock.hb2clock_low, hb2reset)(Module( 
        new HB_Decimator(config=config.hb2_config)
    ))

    val hb3 = withClockAndReset(io.clock.hb3clock_low, hb3reset)(Module(
        new HB_Decimator(config=config.hb3_config)
    ))

    val cic3 = withClockAndReset(io.clock.cic3clockslow, cic3reset)(Module(
        new CIC_Decimator(config=config.cic3_config)
    ))

    //Default is to bypass
    cic3.io.in.clockslow   := io.clock.cic3clockslow
    cic3.io.in.integscale  := io.control.cic3integscale
    cic3.io.in.integshift  := io.control.cic3integshift
    hb1.io.in.clock_low    := io.clock.hb1clock_low
    hb1.io.in.scale        := io.control.hb1scale
    hb2.io.in.clock_low    := io.clock.hb2clock_low
    hb2.io.in.scale        := io.control.hb2scale
    hb3.io.in.clock_low    := io.clock.hb3clock_low
    hb3.io.in.scale        := io.control.hb3scale

    cic3.io.in.iptr_A   := io.in.iptr_A
    hb1.io.in.iptr_A    := cic3.io.out.Z
    hb2.io.in.iptr_A    := hb1.io.out.Z
    hb3.io.in.iptr_A    := hb2.io.out.Z
    io.out.Z            := withClock(io.clock.hb3clock_low){RegNext(io.in.iptr_A) }
   

    //Modes
    when(state === bypass){
            cic3reset           := true.B 
            hb1reset            := true.B
            hb2reset            := true.B
            hb3reset            := true.B
            io.out.Z            := withClock(io.clock.hb3clock_low){RegNext(io.in.iptr_A)}
        }.elsewhen(state === two){
            cic3reset           := true.B 
            hb1reset            := true.B
            hb2reset            := true.B
            hb3reset            := reset.asBool
            hb3.io.in.iptr_A    := io.in.iptr_A
            io.out.Z            := hb3.io.out.Z
        }.elsewhen(state === four){
            cic3reset           := true.B 
            hb1reset            := true.B
            hb2reset            := reset.asBool
            hb3reset            := reset.asBool
            hb2.io.in.iptr_A    := io.in.iptr_A
            hb3.io.in.iptr_A    := hb2.io.out.Z
            io.out.Z            := hb3.io.out.Z
        }.elsewhen(state === eight){
            cic3.reset          := true.B
            hb1.reset           := reset.asBool 
            hb2.reset           := reset.asBool
            hb3reset            := reset.asBool
            hb1.io.in.iptr_A    := io.in.iptr_A
            hb2.io.in.iptr_A    := hb1.io.out.Z
            hb3.io.in.iptr_A    := hb2.io.out.Z
            io.out.Z            := hb3.io.out.Z
        }.elsewhen(state === more){
            cic3reset           := io.control.reset_loop 
            hb1reset            := reset.asBool
            hb2reset            := reset.asBool
            hb3reset            := reset.asBool
            cic3.io.in.iptr_A   := io.in.iptr_A
            hb1.io.in.iptr_A    := cic3.io.out.Z
            hb2.io.in.iptr_A    := hb1.io.out.Z
            hb3.io.in.iptr_A    := hb2.io.out.Z
            io.out.Z            := hb3.io.out.Z
        }
}



/** Generates verilog or sv*/
object F2_Decimator extends App with OptionParser {
    // Parse command-line arguments
    val (options, arguments) = getopts(default_opts, args.toList)
    printopts(options, arguments)

    val f2config_file = options("f2config_file")
    val hb1config_file = options("hb1config_file")
    val hb2config_file = options("hb2config_file")
    val hb3config_file = options("hb3config_file")
    val cic3config_file = options("cic3config_file")
    val target_dir = options("td")
    var f2_config: Option[F2Config] = None

    F2Config.loadFromFiles(f2config_file, hb1config_file, hb2config_file, hb3config_file, cic3config_file) match {
        case Left(config) => {
            f2_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load F2 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    // Generate verilog
    val annos = Seq(ChiselGeneratorAnnotation(() => new F2_Decimator(config=f2_config.get)))
    val sysverilog = (new ChiselStage).emitSystemVerilog(
        new F2_Decimator(config=f2_config.get),
     
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
    "hb1config_file"->"hb_decimator/configs/hb1-config.yml",
    "hb2config_file"->"hb_decimator/configs/hb2-config.yml",
    "hb3config_file"->"hb_decimator/configs/hb3-config.yml",
    "cic3config_file"->"cic_decimator/configs/cic3-config.yml",
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

