package scenarios
import tcof.{Component, _}
import java.time._
import java.time.format._
import java.util.Collection
import java.util.ArrayList

class RunningExample(val now: LocalTime) extends Model {
class Subject(val subjectName: String, val location: String = null, val organisation: String = null, val role: String = null, val shiftName: String = null) extends Component {
name(s"Subject $subjectName")
}


class Resource(val resourceName: String, val internalstate: String = null, val privacylevel: String = null, val valueDouble: Double = 0, val valueInt: Int = 0) extends Component {
name(s"Resource $resourceName")
}


class System extends RootEnsemble {
object testAction extends Ensemble {
val allowedSubjects = components.select[Subject].filter(x => (((x.location != null && x.location.matches("(\\QProduction_Hall\\E)|(\\QProduction_Hall_Section_1\\E)")) && (x.organisation != null && x.organisation.matches("(\\QA\\E)|(\\QASub\\E)")) && (x.role != null && x.role.matches("(\\QWorker\\E)|(\\QWInspector\\E)")) && (x.shiftName == "Shift 1")) || ((x.role != null && x.role.matches("(\\QQA\\E)"))))).map[Component](x => x.getClass().cast(x))
val allowedResources = components.select[Resource].filter(x => (((x.internalstate == "INCIDENT_HAPPENED") && (x.privacylevel != null && x.privacylevel.matches("(PUBLIC)|(RESTRICTED)|(SECRET)|(UNDEFINED)")) && (x.valueInt < 5)) || ((x.valueDouble < 5.0)))).map[Component](x => x.getClass().cast(x))
situation {
((((now isBefore (LocalTime.parse("14:00:00Z", DateTimeFormatter.ISO_OFFSET_TIME))) || (now equals (LocalTime.parse("14:00:00Z", DateTimeFormatter.ISO_OFFSET_TIME)))) && ((now isAfter (LocalTime.parse("06:00:00Z", DateTimeFormatter.ISO_OFFSET_TIME))) || (now equals (LocalTime.parse("06:00:00Z", DateTimeFormatter.ISO_OFFSET_TIME))))))
}
allow(allowedSubjects, "testAction", allowedResources)
}

val testActionRule = rules(testAction)


}
val rootEnsemble = root(new System)

}
object RunningExample {
def convertToCol(iterable: Iterable[Component]) : Collection[Component] = {
val collection = new ArrayList[Component]

val iter = iterable.iterator
while (iter.hasNext) {
collection.add(iter.next)
}

return collection
}

def main(args: Array[String]) : Unit = {
//TODO: adapt to your usecase scenario
val scenario = new RunningExample(LocalTime.parse("13:00:00Z", DateTimeFormatter.ISO_OFFSET_TIME))
val subjectA = new scenario.Subject("A", "Production_Hall_Section_1", "ASub", "Worker", "Shift 1")
val subjectB = new scenario.Subject("B", "Shift 2")
val resourceA = new scenario.Resource("machine", "INCIDENT_HAPPENED", "PUBLIC", 5, 4)
scenario.components = List(subjectA, subjectB, resourceA)
scenario.rootEnsemble.init()
scenario.rootEnsemble.solve()
val testActionAllow = scenario.rootEnsemble.instance.testActionRule.selectedMembers.exists(x => convertToCol(x.allowedSubjects).contains(subjectA) && !convertToCol(x.allowedSubjects).contains(subjectB))
if(testActionAllow) {
println("allow")
} else {
println("deny")
}
}

}
