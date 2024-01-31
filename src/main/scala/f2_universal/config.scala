// See LICENSE_AALTO.txt for license details

package f2_universal.config

import net.jcazevedo.moultingyaml._
import net.jcazevedo.moultingyaml.DefaultYamlProtocol._
import scala.math.BigInt
import scala.io.Source
import chisel3._

import hb_universal.config.{hbConfig}
import cic_universal.config.{cicConfig}

case class f2Generic(
  syntax_version:     Option[Int], // None for scala instantiation
  resolution:         Int,
  gainBits:           Int
)

case class f2Config(
  syntax_version:     Option[Int], // None for scala instantiation
  resolution:         Int,
  gainBits:           Int,
  hb1_config:         hbConfig,
  hb2_config:         hbConfig,
  hb3_config:         hbConfig,
  cic3_config:        cicConfig
)

object f2Config {
  implicit val f2GenericFormat = yamlFormat3(f2Generic)

  // TODO: Update this to always match the major version number of the release
  val syntaxVersion = 2

  /** Exception type for FIR config parsing errors */
  class f2ConfigParseException(msg: String) extends Exception(msg)

  /** Type for representing error return values from a function */
  case class Error(msg: String) {
    /** Throw a parsing exception with a debug message. */
    def except() = { throw new f2ConfigParseException(msg) }

    /** Abort program execution and print out the reason */
    def panic() = {
      System.err.println(msg)
      System.exit(-1)
    }
  }

  /** parse legal syntax version from config yaml AST */
  private[config] def parseSyntaxVersion(yamlAst: YamlValue): Either[BigInt,Error] = {
    // get version number as an integer
    val version: BigInt = yamlAst.asYamlObject.fields.get(YamlString("syntax_version")) match {
      case Some(version) => version match {
        case maybeDecimal: YamlNumber => maybeDecimal.asInstanceOf[YamlNumber].value.toBigIntExact match {
          case Some(integer) => integer
          case None => return Right(Error(s"Top-level key `syntax_version` must have an integer value. $version is not!"))
        }
        case _ => return return Right(Error(s"Top-level key `syntax_version` must have an integer value. $version is not!"))
      }
      case None => return Right(Error("Missing required top-level key: `syntax_version`."))
    }
    if (syntaxVersion != version)
      return Right(Error(s"Unsupported syntax version: $version.\n- Supported versions: $syntaxVersion"))
    Left(version)
  }

  def loadFromFile(f2_file: String,
    hb1_file: String, 
    hb2_file: String, 
    hb3_file: String, 
    cic3_file: String
    ): Either[f2Config, Error] = {

    println(s"\nLoading f2 configuration from file: $f2_file")
    var f2_fileString: String = ""
    try {
      val bufferedSource = Source.fromFile(f2_file)
      f2_fileString = bufferedSource.getLines().mkString("\n")
      bufferedSource.close
    } catch {
      case e: Exception => return Right(Error(e.getMessage()))
    }
    
    // print file contents as troubleshooting info
    println("\nYAML configuration file contents:")
    //println(s"```\n$fileString\n```")

    // Determine syntax version
    val f2yamlAst = f2_fileString.parseYaml

    val syntaxVersion = parseSyntaxVersion(f2yamlAst)
    syntaxVersion match {
      case Left(value) => ()
      case Right(err) => return Right(err)
    }

    // Parse FirConfig from YAML AST
    val f2_config = f2yamlAst.convertTo[f2Generic]

    println("resolution:")
    println(f2_config.resolution)

    println("gainBits:")
    println(f2_config.gainBits)

    var hb1_config: Option[hbConfig] = None
    var hb2_config: Option[hbConfig] = None
    var hb3_config: Option[hbConfig] = None
    var cic3_config: Option[cicConfig] = None

    hbConfig.loadFromFile(hb1_file) match {
        case Left(config) => {
            hb1_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load f2 hb1 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    hbConfig.loadFromFile(hb2_file) match {
        case Left(config) => {
            hb2_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load f2 hb2 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    hbConfig.loadFromFile(hb3_file) match {
        case Left(config) => {
            hb3_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load f2 hb3 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    cicConfig.loadFromFile(cic3_file) match {
        case Left(config) => {
            cic3_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load f2 cic3 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    val config = new f2Config(
	    f2_config.syntax_version, 
	    f2_config.resolution, 
	    f2_config.gainBits,
        hb1_config.get,
        hb2_config.get,
        hb3_config.get,
        cic3_config.get
    )

    Left(config)
  }
}
