import java.util

import akka.util.ByteString
import followers.model.Event
import followers.model.Event.Follow

import scala.collection.SortedSet


List(Set(1,2), Set(2,3)).reduce(_ ++ _)