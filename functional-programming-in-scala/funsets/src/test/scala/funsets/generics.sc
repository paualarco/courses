import scala.reflect._

def recursiveAddList[T](list: List[T])(implicit tag: ClassTag[T]): T =
  list match {
    case head :: Nil if tag == classTag[String] =>
      head.asInstanceOf[String].toLowerCase.asInstanceOf[T]
    case head :: Nil if tag == classTag[Int] =>
      (head.asInstanceOf[Int] * 2).asInstanceOf[T]
    case head :: tail if tag == classTag[String] =>
      (head.asInstanceOf[String].toLowerCase ++ recursiveAddList(tail).asInstanceOf[String]).asInstanceOf[T]
    case head :: tail if tag == classTag[Int] =>
      ((head.asInstanceOf[Int] * 2) + recursiveAddList(tail).asInstanceOf[Int]).asInstanceOf[T]
    case _ => throw new RuntimeException(s"Don't know how to handle $tag")
  }

recursiveAddList(List(1, 2, 3))

recursiveAddList(List("A", "b", "CdE"))

def stableIdentifierPattern[T](n: Any)(implicit tag: ClassTag[T]) : T = {
  n.asInstanceOf[T]
}





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
  val deltaProp = builder get "Pepito"
}
class DeltaCompactionConfiguration(inputDay: String, rootDir: String, builder: ConfigurationBuilder) extends DeltaConfiguration(inputDay, rootDir, builder)  {
  override val confProp = builder get "Delta Compaction"
}
class NRTConfiguration(inputDay: String, rootDir: String, builder: ConfigurationBuilder) extends BasicConfiguration(inputDay, rootDir, builder) {
  val confProp = builder get "NRT"
}


type Row = Int
class ConfigurationFactory[I<:BasicConfiguration](inputDay: String = "9999-12-31", rootDir: String = "root") {

  val BasicType = classTag[BasicConfiguration]
  val DeltaType = classTag[DeltaConfiguration]
  val DeltaCompactionType = classTag[DeltaCompactionConfiguration]
  val NRTType = classTag[NRTConfiguration]

  private[this] def selectConf[T](confBuilder: ConfigurationBuilder, tag: Object) = {

    tag match {
      case DeltaType => println("Delta"); new DeltaConfiguration(inputDay, rootDir, confBuilder)
      case DeltaCompactionType => println("DeltaComp"); new DeltaCompactionConfiguration(inputDay, rootDir, confBuilder)
      case NRTType => println("NRT"); new NRTConfiguration(inputDay, rootDir, confBuilder)
      case _ => println("Basic"); new BasicConfiguration(inputDay, rootDir, confBuilder)
    }
  }

  def createConfFromCSV[T](csvName: String)(implicit tag: ClassTag[T]): T = {
    selectConf(new CsvConfigurationBuilder(csvName), tag).asInstanceOf[T]
  }

  def createConfFromRow[T](row: Row)(implicit tag: ClassTag[T]): T = {
    selectConf(new RowConfigurationBuilder(row), tag).asInstanceOf[T]
  }
}

val configurationFactory = new ConfigurationFactory("2019-03-28", "/PRBBD").createConfFromCSV[DeltaConfiguration]("csvName").deltaProp



