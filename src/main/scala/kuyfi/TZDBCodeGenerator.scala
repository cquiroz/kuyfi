package kuyfi

import shapeless._

import scalaz.effect._
import TZDB._
import treehugger.forest._
import definitions._
import treehuggerDSL._
import java.time.{DayOfWeek, ZoneOffset, LocalDateTime, LocalDate, LocalTime, Month}
import java.time.zone.ZoneOffsetTransitionRule
import java.time.zone.ZoneOffsetTransitionRule.TimeDefinition

object TZDBCodeGenerator {
  val groupingSize = 3

  private val autoGeneratedCommend = "Auto-generated code from TZDB definitions, don't edit"

  val zoneOffsetSym: Symbol = getModule("ZoneOffset")
  val zoneOffsetTransitionSym: Symbol = getModule("ZoneOffsetTransition")
  val zoneOffsetTransitionRuleSym: Symbol = getModule("ZoneOffsetTransitionRule")
  val zoneRulesSym: Symbol = getModule("ZoneRules")
  val localDateTimeSym: Symbol = getModule("LocalDateTime")
  val localTimeSym: Symbol = getModule("LocalTime")

  // Typeclass of code generator
  trait TreeGenerator[A] {
    def generateTree(a: A): Tree
  }

  implicit class TreeGeneratorOps[A](val a: A) extends AnyVal {
    def toTree(implicit t: TreeGenerator[A]): Tree = t.generateTree(a)
  }

  object TreeGenerator {
    // "Summoner" method
    def apply[A](implicit enc: TreeGenerator[A]): TreeGenerator[A] = enc

    // "Constructor" method
    def instance[A](func: A => Tree): TreeGenerator[A] =
      new TreeGenerator[A] {
        def generateTree(value: A): Tree =
          func(value)
    }

    // Globally visible type class instances
    implicit def intInstance: TreeGenerator[Int] = instance(LIT.apply)
    implicit def stringInstance: TreeGenerator[String] = instance(LIT.apply)

    // Encoders for products
    implicit def cnilInstance: TreeGenerator[CNil] = instance(_ => throw new Exception("Cannot happen"))
    implicit def coproductInstance[H, T <: Coproduct](
      implicit
      hInstance: Lazy[TreeGenerator[H]], // wrap in Lazy
      tInstance: TreeGenerator[T]
    ): TreeGenerator[H :+: T] = instance {
      case Inl(h) => hInstance.value.generateTree(h)
      case Inr(t) => tInstance.generateTree(t)
    }
  }

  object PureTreeGenerator {
    implicit val zoneInstance: TreeGenerator[Zone] =
      TreeGenerator.instance(z =>
        TUPLE(LIT(z.name), REF(z.scalaSafeName))
      )

    implicit val linkInstance: TreeGenerator[Link] =
      TreeGenerator.instance(l => TUPLE(l.to.toTree, l.from.toTree))

    implicit val zoneListInstance: TreeGenerator[List[Zone]] =
      TreeGenerator.instance( z =>
        LAZYVAL("allZones", "Map[String, ZoneRules]") := MAKE_MAP(z.map(_.toTree))
      )

    implicit val stdListInstance: TreeGenerator[List[(Zone, StandardRulesParams)]] =
      TreeGenerator.instance{ l =>
        LAZYVAL("stdZones", TYPE_MAP(StringClass, TYPE_REF("ZR"))) := MAKE_MAP(l.map { case (z, _) => TUPLE(List(LIT(z.name), REF(z.scalaSafeName)): _*)})
      }

    implicit val fixedListInstance: TreeGenerator[List[(Zone, FixedZoneRulesParams)]] =
      TreeGenerator.instance( l =>
        LAZYVAL("fixedZones", TYPE_MAP(StringClass, TYPE_REF("ZF"))) := MAKE_MAP(l.map { case (z, _) => TUPLE(List(LIT(z.name), REF("rules." + z.scalaSafeName)): _*)})
      )

    implicit val linkInstances: TreeGenerator[List[Link]] =
      TreeGenerator.instance( l =>
        LAZYVAL("zoneLinks", "Map[String, String]") := MAKE_MAP(l.map(_.toTree): _*)
      )

    implicit val zoneOffsetInstance: TreeGenerator[ZoneOffset] =
      TreeGenerator.instance( l =>
        (zoneOffsetSym DOT "ofTotalSeconds")(LIT(l.getTotalSeconds))
      )

    implicit val dayOfWeekInstance: TreeGenerator[DayOfWeek] =
      TreeGenerator.instance(l => REF(s"DayOfWeek.${l.toString}"))

    implicit val timeDefinitionInstance: TreeGenerator[TimeDefinition] =
      TreeGenerator.instance(l => REF(s"ZoneOffsetTransitionRule.TimeDefinition.${l.toString}"))

    implicit val monthInstance: TreeGenerator[Month] =
      TreeGenerator.instance(l => REF(s"Month.${l.toString}"))

    implicit val localDateTimeInstance: TreeGenerator[LocalDateTime] =
      TreeGenerator.instance( l =>
        (localDateTimeSym DOT "of")(LIT(l.getYear), LIT(l.getMonthValue), LIT(l.getDayOfMonth), LIT(l.getHour), LIT(l.getMinute), LIT(l.getSecond), LIT(l.getNano))
      )

    implicit val localTimeInstance: TreeGenerator[LocalTime] =
      TreeGenerator.instance( l =>
        (localTimeSym DOT "of")(LIT(l.getHour), LIT(l.getMinute), LIT(l.getSecond), LIT(l.getNano))
      )

    implicit val ZoneOffsetTransitionParamsInstance: TreeGenerator[ZoneOffsetTransitionParams] =
      TreeGenerator.instance( l =>
        (zoneOffsetTransitionSym DOT "of")(l.transition.toTree, l.offsetBefore.toTree, l.offsetAfter.toTree)
      )

    implicit val ZoneOffsetTransitionRuleInstance: TreeGenerator[ZoneOffsetTransitionRule] =
      TreeGenerator.instance { l =>
        val dayOfWeek = Option(l.getDayOfWeek).map(_.toTree).getOrElse(NULL)
        (zoneOffsetTransitionRuleSym DOT "of")(l.getMonth.toTree, LIT(l.getDayOfMonthIndicator), dayOfWeek, l.getLocalTime.toTree, LIT(l.isMidnightEndOfDay), l.getTimeDefinition.toTree, l.getStandardOffset.toTree, l.getOffsetBefore.toTree, l.getOffsetAfter.toTree)
      }

    implicit val zoneAndRules: TreeGenerator[Map[Zone, ZoneRulesParams]] =
      TreeGenerator.instance { zones => BLOCK(zones.map {
        case (z, r) =>
          LAZYVAL(z.scalaSafeName, zoneRulesSym) := {
            r.toTree
          }
        })
      }

    implicit val zoneStdTupleRules: TreeGenerator[(Zone, FixedZoneRulesParams)] =
      TreeGenerator.instance {
        case (z, r) =>
          implicitly[TreeGenerator[(Zone, ZoneRulesParams)]]
          LAZYVAL(z.scalaSafeName, TYPE_REF("ZF")) := {
            r.toTree
          }
      }

    implicit val zoneFixedTupleRules: TreeGenerator[(Zone, StandardRulesParams)] =
      TreeGenerator.instance {
        case (z, r) =>
          implicitly[TreeGenerator[(Zone, ZoneRulesParams)]]
          LAZYVAL(z.scalaSafeName, TYPE_REF("ZoneRules")) := {
            r.toTree
          }
      }

    implicit val zoneTupleRules: TreeGenerator[(Zone, ZoneRulesParams)] =
      TreeGenerator.instance {
        case (z, r) =>
          LAZYVAL(z.scalaSafeName, TYPE_REF("ZoneRules")) := {
            r.toTree
          }
        }

    implicit val zoneRules: TreeGenerator[ZoneRulesParams] =
      TreeGenerator.instance {
        case f: FixedZoneRulesParams => f.toTree
        case s: StandardRulesParams => s.toTree
      }

    implicit val fixedZoneRules: TreeGenerator[FixedZoneRulesParams] =
      TreeGenerator.instance{ l =>
        val at = l.baseStandardOffset.getTotalSeconds
        val offset = (zoneOffsetSym DOT "ofTotalSeconds")(LIT(at))
        (zoneRulesSym DOT "of")(offset)
      }

    implicit val standardZoneRules: TreeGenerator[StandardRulesParams] =
      TreeGenerator.instance( l =>
        BLOCK(List(
          VAL("bso", "ZoneOffset") := l.baseStandardOffset.toTree,
          VAL("bwo", "ZoneOffset") := l.baseWallOffset.toTree,
          VAL("standardTransitions", "List[ZoneOffsetTransition]") := LIST(l.standardOffsetTransitionList.map(_.toTree)),
          VAL("transitionList", "List[ZoneOffsetTransition]") := LIST(l.transitionList.map(_.toTree)),
          VAL("lastRules", "List[ZoneOffsetTransitionRule]") := LIST(l.lastRules.map(_.toTree)),
          (zoneRulesSym DOT "of")(REF("bso"), REF("bwo"), REF("standardTransitions") POSTFIX "asJava", REF("transitionList") POSTFIX "asJava", REF("lastRules") POSTFIX "asJava")
        ))
      )
  }

  object OptimizedTreeGenerator {
    def zoneOffsetSafeName(zo: Int): String = s"zo_${if (zo <0) s"_${-zo}" else zo.toString}"
    def zoneRuleSafeName(zo: ZoneOffsetTransitionParams): String = s"zot_${if (zo.hashCode < 0) s"_${-zo.hashCode}" else zo.hashCode}"

    val JsListClass          =  definitions.getClass("scala.scalajs.js.Array")
    def TYPE_JSLIST(typ: Type): Type    = JsListClass TYPE_OF typ

    def JSLIST(typ: Type, xs: Iterable[Tree]): Tree    = TYPE_JSLIST(typ) APPLY xs

    implicit val zoneInstance: TreeGenerator[Zone] =
      TreeGenerator.instance(z =>
        TUPLE(LIT(z.name), REF(s"rules.${z.scalaSafeName}"))
      )

    implicit val linkInstance: TreeGenerator[Link] =
      TreeGenerator.instance(l => TUPLE(l.to.toTree, l.from.toTree))

    implicit val stdListInstance: TreeGenerator[List[(Zone, StandardRulesParams)]] =
      TreeGenerator.instance{ l =>
        LAZYVAL("stdZones", TYPE_REF("scala.scalajs.js.Dynamic")) := REF("js.Dynamic.literal") APPLY l.map { case (z, u) => TUPLE(List(LIT(z.name), REF(z.scalaGroup(groupingSize) + "." + z.scalaSafeName)): _*) }
      }

    implicit val fixedListInstance: TreeGenerator[List[(Zone, FixedZoneRulesParams)]] =
      TreeGenerator.instance( l =>
        LAZYVAL("fixedZones", TYPE_REF("scala.scalajs.js.Dynamic")) := REF("js.Dynamic.literal") APPLY l.map { case (z, f) => TUPLE(List(LIT(z.name), LIT(f.baseStandardOffset.getTotalSeconds))) }
      )

    implicit val linkInstances: TreeGenerator[List[Link]] =
      TreeGenerator.instance( l =>
        LAZYVAL("zoneLinks", TYPE_MAP(StringClass, StringClass)) := MAKE_MAP(l.map(_.toTree): _*)
      )

    implicit val zoneOffsetInstance: TreeGenerator[ZoneOffset] =
      TreeGenerator.instance( l =>
        VAL(zoneOffsetSafeName(l.getTotalSeconds), IntClass) := LIT(l.getTotalSeconds)
      )

    implicit val dayOfWeekInstance: TreeGenerator[DayOfWeek] =
      TreeGenerator.instance(l => LIT(l.getValue))

    implicit val timeDefinitionInstance: TreeGenerator[TimeDefinition] =
      TreeGenerator.instance(l => LIT(l.ordinal))

    implicit val monthInstance: TreeGenerator[Month] =
      TreeGenerator.instance(l => LIT(l.getValue))

    implicit val localDateTimeInstance: TreeGenerator[LocalDateTime] =
      TreeGenerator.instance( l =>
        JSLIST(IntClass, List(LIT(l.toLocalDate.getYear), LIT(l.toLocalDate.getDayOfYear), LIT(l.toLocalTime.toSecondOfDay)))
      )

    implicit val localTimeInstance: TreeGenerator[LocalTime] =
      TreeGenerator.instance( l =>
        LIT(l.toSecondOfDay)
      )

    implicit val localDateInstance: TreeGenerator[LocalDate] =
      TreeGenerator.instance { l =>
        val ys = f"${l.getYear}%04d"
        val ds = f"${l.getDayOfYear}%03d"
        JSLIST(IntClass, List(LIT((ys + ds).toInt)))
      }

    implicit val ZoneOffsetTransitionParamsInstance: TreeGenerator[ZoneOffsetTransitionParams] =
      TreeGenerator.instance { l =>
        val ys = f"${l.transition.toLocalDate.getYear}%04d"
        val ds = f"${l.transition.toLocalDate.getDayOfYear}%03d"
        JSLIST(IntClass, List(
          LIT((ys + ds).toInt),
          LIT(l.transition.toLocalTime.toSecondOfDay),
          LIT(l.offsetBefore.getTotalSeconds),
          LIT(l.offsetAfter.getTotalSeconds)))
      }

    implicit val ZoneOffsetTransitionRuleInstance: TreeGenerator[ZoneOffsetTransitionRule] =
      TreeGenerator.instance { l =>
        //val dayOfWeek = Option(l.getDayOfWeek).fold(NONE)(x => SOME(x.toTree))
        val dayOfWeek = Option(l.getDayOfWeek).fold(LIT(-1))(x => LIT(x.getValue))
        //TUPLE(l.getMonth.toTree, LIT(l.getDayOfMonthIndicator), dayOfWeek, l.getLocalTime.toTree, LIT(l.isMidnightEndOfDay), l.getTimeDefinition.toTree, REF("zo." + zoneOffsetSafeName(l.getStandardOffset.getTotalSeconds)), REF("zo." + zoneOffsetSafeName(l.getOffsetBefore.getTotalSeconds)), REF("zo." + zoneOffsetSafeName(l.getOffsetAfter.getTotalSeconds)))
        JSLIST(IntClass, List(
          LIT(l.getMonth.getValue),
          LIT(l.getDayOfMonthIndicator),
          dayOfWeek,
          l.getLocalTime.toTree,
          LIT(if (l.isMidnightEndOfDay) 1 else 0),
          l.getTimeDefinition.toTree,
          LIT(l.getStandardOffset.getTotalSeconds),
          LIT(l.getOffsetBefore.getTotalSeconds),
          LIT(l.getOffsetAfter.getTotalSeconds)))
      }

    implicit val zoneStdTupleRules: TreeGenerator[(Zone, FixedZoneRulesParams)] =
      TreeGenerator.instance {
        case (z, r) =>
          implicitly[TreeGenerator[(Zone, ZoneRulesParams)]]
          LAZYVAL(z.scalaSafeName, TYPE_REF("ZF")) := {
            r.toTree
          }
      }

    implicit val zoneFixedTupleRules: TreeGenerator[(Zone, StandardRulesParams)] =
      TreeGenerator.instance {
        case (z, r) =>
          implicitly[TreeGenerator[(Zone, ZoneRulesParams)]]
          VAL(z.scalaSafeName, TYPE_REF("scala.scalajs.js.Dynamic")) := {
            r.toTree
          }
      }

    implicit val zoneTupleRules: TreeGenerator[(Zone, ZoneRulesParams)] =
      TreeGenerator.instance {
        case (z, r: StandardRulesParams) =>
          LAZYVAL(z.scalaSafeName, TYPE_REF("")) := {
            r.toTree
          }
        case (z, r: FixedZoneRulesParams) =>
          LAZYVAL(z.scalaSafeName, TYPE_REF("ZF")) := {
            r.toTree
          }
      }

    implicit val zoneRules: TreeGenerator[ZoneRulesParams] =
      TreeGenerator.instance {
        case f: FixedZoneRulesParams => f.toTree
        case s: StandardRulesParams => s.toTree
      }

    implicit val fixedZoneRules: TreeGenerator[FixedZoneRulesParams] =
      TreeGenerator.instance{ l =>
        LIT(l.baseStandardOffset.getTotalSeconds)
      }

    implicit val standardZoneRules: TreeGenerator[StandardRulesParams] =
      TreeGenerator.instance( l =>
        // This will build a literal value for the array
        REF("js.Dynamic.literal") APPLY(
          TUPLE(LIT("s"), LIT(l.baseStandardOffset.getTotalSeconds)),
          TUPLE(LIT("w"), LIT(l.baseWallOffset.getTotalSeconds)),
          TUPLE(LIT("t"), JSLIST(TYPE_JSLIST(IntClass), l.standardOffsetTransitionList.map(r => r.toTree))),
          TUPLE(LIT("l"), JSLIST(TYPE_JSLIST(IntClass), l.transitionList.map(r => r.toTree))),
          TUPLE(LIT("r"), JSLIST(TYPE_JSLIST(IntClass), l.lastRules.map(_.toTree)))
      ))
  }

  // Go over the links and remove links whose source is unknown
  def cleanLinks(rows: List[Row]): List[Row] ={
    val zoneNames = rows.flatMap(_.select[Zone]).map(_.name)
    rows.filter {
      case r if r.select[Link].isDefined => r.select[Link].exists(l => zoneNames.contains(l.from))
      case _                             => true
    }
  }

  def exportTzdb(tzdbPackage: String, importsPackage: String, rows: List[Row])
    (implicit genRuleMap: TreeGenerator[(Zone, StandardRulesParams)], genFixedList: TreeGenerator[List[(Zone, FixedZoneRulesParams)]], genStdList: TreeGenerator[List[(Zone, StandardRulesParams)]], genLinks: TreeGenerator[List[Link]]): Tree = {
    val rules = ZoneRulesBuilder.calculateTransitionParams(rows)
    // Fixed zone rules
    val fixed: List[(Zone, FixedZoneRulesParams)] = rules.collect {
      case (z, r: FixedZoneRulesParams) => (z, r)
    }.toList
    // Standard zone rules
    val standard: List[(Zone, StandardRulesParams)] = rules.collect {
      case (z, r: StandardRulesParams) => (z, r)
    }.toList

    val zoneProviderSym = getModule("ZoneRulesProvider")
    val register = (zoneProviderSym DOT "registerProvider")(NULL)

    // Split standard rules
    val groupedRules: List[Tree] = standard.groupBy(_._1.scalaGroup(groupingSize)).map {
      case (groupName, blockRules) =>
        OBJECTDEF(groupName) := BLOCK(blockRules.map(_.toTree))
    }.toList


    BLOCK (
      List(
        IMPORT(s"$importsPackage._"),
        IMPORT(s"$importsPackage.zone._"),
        IMPORT("scala.collection.JavaConverters._"),
        IMPORT("scala.language.postfixOps"),
        IMPORT("scala.scalajs.js"),
        OBJECTDEF("tzdb") := BLOCK(groupedRules ::: List(fixed.toTree, standard.toTree, rows.flatMap(_.select[Link]).toTree)))
    ) inPackage tzdbPackage withComment autoGeneratedCommend
  }

  def exportAll(dir: java.io.File, to: java.io.File, packageName: String, importsPackage: String)
    (implicit genRuleMap: TreeGenerator[(Zone, StandardRulesParams)], genFixedList: TreeGenerator[List[(Zone, FixedZoneRulesParams)]], genStdList: TreeGenerator[List[(Zone, StandardRulesParams)]], genLinks: TreeGenerator[List[Link]]): IO[Unit] = {
    import better.files._
    for {
      rows      <- TZDBParser.parseAll(File(dir.toURI))
      tree      <- IO(exportTzdb(packageName, importsPackage, rows))
      _         <- IO(File(to.toURI).write(treeToString(tree)))
    } yield ()
  }

  // Add to generated code. It isn't worth generating this with treehugger
  val paramsToRules = Nil
}
