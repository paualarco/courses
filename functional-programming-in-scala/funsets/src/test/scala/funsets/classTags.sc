
import scala.reflect._

/*
val StringClass = classTag[String]
val IntClass = classTag[Int]
def typeList[T](list: List[T])(implicit tag: ClassTag[T]) =
  tag match {
    case StringClass => "It's a String!"
    case IntClass => "It's an Integer."
    case _ => "It's something else entirely"
  }

typeList(List(1,2,3))
*/

class A(s:String){ val prop = "A"}
class B(s:String) extends A(s) { override val prop = "B"}
class C(s:String) extends A(s) { override val prop = "C"}

val A_ = classTag[A]
val B_ = classTag[B]
def typeList[T](t:String)(implicit tag: ClassTag[T]) =
  tag match {
    case A_ => "It's A!"
    case B_ => "It's B."
    case _ => "It's something else entirely"
  }

typeList[A]("hi")



import com.sun.org.apache.bcel.internal.generic.BasicType
import org.scalatest.prop.Configuration


trait ConfigurationBuilder {
  def get(property: String): String
}

class CsvConfigurationBuilder(csvName: String) extends ConfigurationBuilder{
  override def get(property: String): String = property
}

class RowConfigurationBuilder(row: Row) extends ConfigurationBuilder{
  override def get(property: String): String = row.toString
}


class BasicConfiguration(inputDay: String, rootDir: String, builder: ConfigurationBuilder)  {
  val prop = builder get "haha"
}

class DeltaConfiguration(inputDay: String, rootDir: String, builder: ConfigurationBuilder) extends BasicConfiguration(inputDay, rootDir, builder) {
  val confProp = builder get "Delta"
}
class DeltaCompactionConfiguration(inputDay: String, rootDir: String, builder: ConfigurationBuilder) extends DeltaConfiguration(inputDay, rootDir, builder)  {
  override val confProp = builder get "Delta Compaction"
}
class NRTConfiguration(inputDay: String, rootDir: String, builder: ConfigurationBuilder) extends BasicConfiguration(inputDay, rootDir, builder) {
  val confProp = builder get "NRT"
}


type Row = Int
class ConfigurationFactory[I<:BasicConfiguration](inputDay: String = "9999-12-31", rootDir: String = "root")(implicit tag: ClassTag[I]) {

  val BasicType = classTag[BasicConfiguration]
  val DeltaType = classTag[DeltaConfiguration]
  val DeltaCompactionType = classTag[DeltaCompactionConfiguration]
  val NRTType = classTag[NRTConfiguration]

  private[this] def selectConf(confBuilder: ConfigurationBuilder): BasicConfiguration = {
    tag match {
      case DeltaType => println("Delta"); new DeltaConfiguration(inputDay, rootDir, confBuilder)
      case DeltaCompactionType => println("DeltaComp"); new DeltaCompactionConfiguration(inputDay, rootDir, confBuilder)
      case NRTType => println("NRT"); new NRTConfiguration(inputDay, rootDir, confBuilder)
      case _ => println("Basic"); new BasicConfiguration(inputDay, rootDir, confBuilder)
    }
  }

  def createConfFromCSV(csvName: String) = {
    selectConf(new CsvConfigurationBuilder(csvName))
  }

  def createConfFromRow(row: Row) = {
    selectConf(new RowConfigurationBuilder(row))
  }
}

val configurationFactory = new ConfigurationFactory[NRTConfiguration]("2019-03-28", "/PRBBD").createConfFromCSV("csvName").asInstanceOf[NRTConfiguration]




