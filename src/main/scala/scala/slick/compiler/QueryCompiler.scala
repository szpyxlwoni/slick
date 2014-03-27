package scala.slick.compiler

import scala.collection.immutable.HashMap
import scala.slick.SlickException
import scala.slick.util.Logging
import scala.slick.ast.{SymbolNamer, Node}

/** An immutable, stateless query compiler consisting of a series of phases */
class QueryCompiler(val phases: Vector[Phase]) extends Logging {

  /** Return a new compiler with the new phase added at the end. */
  def + (p: Phase) = new QueryCompiler(phases :+ p)

  /** Return a new compiler with the new phase added directly after another
   * phase (or a different implementation of the same phase name). */
  def addAfter(p: Phase, after: Phase) = new QueryCompiler({
    val i = phases.lastIndexWhere(_.name == after.name)
    if(i == -1) throw new SlickException("Previous phase "+after.name+" not found")
    else phases.patch(i+1, Seq(p), 0)
  })

  /** Return a new compiler with the new phase added directly before another
    * phase (or a different implementation of the same phase name). */
  def addBefore(p: Phase, before: Phase) = new QueryCompiler({
    val i = phases.indexWhere(_.name == before.name)
    if(i == -1) throw new SlickException("Following phase "+before.name+" not found")
    else phases.patch(i, Seq(p), 0)
  })

  /** Return a new compiler without the given phase (or a different
   * implementation of the same phase name. */
  def - (p: Phase) = new QueryCompiler(phases.filterNot(_.name == p.name))

  /** Return a new compiler that replaces an existing phase by a new one with
   * the same name. The new phase must have a State that is assignable to the
   * original phase's state. */
  def replace(p: Phase) = new QueryCompiler(phases.map(o => if(o.name == p.name) p else o))

  /** Compile an AST with a new `CompilerState`. */
  def run(tree: Node): CompilerState = {
    val state = new CompilerState(this, tree)
    run(state)
  }

  /** Compile an AST in an existing `CompilerState`. This can be used for triggering
    * compilation of subtrees within the current `CompilerState`. */
  def run(state: CompilerState): CompilerState = {
    if(logger.isDebugEnabled) state.symbolNamer.use { logger.debug("Source:", state.tree) }
    phases.foldLeft(state){ case (n,p) => runPhase(p, n) }
  }

  /** Compile an AST in an existing `CompilerState`, stopping just before the specified phase.
    * This can be used for triggering compilation of subtrees within the current `CompilerState`. */
  def runBefore(before: Phase, state: CompilerState): CompilerState = {
    if(logger.isDebugEnabled) state.symbolNamer.use { logger.debug("Source:", state.tree) }
    phases.iterator.takeWhile(_.name != before.name).foldLeft(state){ case (n,p) => runPhase(p, n) }
  }

  protected[this] def runPhase(p: Phase, state: CompilerState): CompilerState = state.symbolNamer.use {
    val s2 = p(state)
    if(s2.tree ne state.tree) logger.debug("After phase "+p.name+":", s2.tree)
    else logger.debug("After phase "+p.name+": (no change)")
    s2
  }
}

object QueryCompiler {
  /** The standard phases of the query compiler */
  val standardPhases = Vector(
    // Clean up trees from the lifted embedding
    Phase.inline,
    Phase.assignUniqueSymbols,
    // Distribute and normalize
    Phase.inferTypes,
    Phase.createResultSetMapping,
    Phase.forceOuterBinds,
    // Convert to column form
    Phase.expandTables,
    Phase.expandRecords,
    Phase.flattenProjections,
    Phase.relabelUnions,
    Phase.pruneFields,
    Phase.assignTypes
  )

  /** Extra phases for translation to SQL comprehensions */
  val relationalPhases = Vector(
    Phase.resolveZipJoins,
    Phase.convertToComprehensions,
    Phase.fuseComprehensions,
    Phase.fixRowNumberOrdering,
    Phase.hoistClientOps
  )

  /** The default compiler */
  val standard = new QueryCompiler(standardPhases)

  /** The default compiler with the additional conversion to relational trees */
  val relational = new QueryCompiler(standardPhases ++ relationalPhases)

  /** Construct a new `QueryCompiler` with the given phases */
  def apply(phases: Phase*) = new QueryCompiler(phases.toVector)
}

/** A phase of the query compiler, identified by a unique name */
trait Phase extends (CompilerState => CompilerState) with Logging {
  /** The immutable state of the phase that can also be accessed by other phases. */
  type State

  /** The unique name of the phase */
  val name: String

  /** Run the phase */
  def apply(state: CompilerState): CompilerState
}

/** The `Phase` companion objects contains ready-to-use `Phase` objects for
  * the standard phases of the query compiler */
object Phase {
  val inline = new Inline
  val assignUniqueSymbols = new AssignUniqueSymbols
  val inferTypes = new InferTypes
  val createResultSetMapping = new CreateResultSetMapping
  val forceOuterBinds = new ForceOuterBinds
  val expandTables = new ExpandTables
  val expandRecords = new ExpandRecords
  val flattenProjections = new FlattenProjections
  val relabelUnions = new RelabelUnions
  val pruneFields = new PruneFields
  val resolveZipJoins = new ResolveZipJoins
  val assignTypes = new AssignTypes
  val convertToComprehensions = new ConvertToComprehensions
  val fuseComprehensions = new FuseComprehensions
  val fixRowNumberOrdering = new FixRowNumberOrdering
  val hoistClientOps = new HoistClientOps

  /* Extra phases that are not enabled by default */
  val rewriteBooleans = new RewriteBooleans
}

/** The current state of a compiler run, consisting of the current AST and
  * additional immutable state of individual phases. Mutability is confined
  * to the SymbolNamer. The state is tied to a specific compiler instance so
  * that phases can call back into the compiler. */
class CompilerState private (val compiler: QueryCompiler, val symbolNamer: SymbolNamer,
                             val tree: Node, state: HashMap[String, Any]) {
  def this(compiler: QueryCompiler, tree: Node) =
    this(compiler, new SymbolNamer("s", "t"), tree, new HashMap)

  /** Get the phase state for a phase */
  def get[P <: Phase](p: P): Option[p.State] = state.get(p.name).asInstanceOf[Option[p.State]]

  /** Return a new `CompilerState` with the given mapping of phase to phase state */
  def + [S, P <: Phase { type State = S }](t: (P, S)) =
    new CompilerState(compiler, symbolNamer, tree, state + (t._1.name -> t._2))

  /** Return a new `CompilerState` which encapsulates the specified AST */
  def withNode(n: Node) = new CompilerState(compiler, symbolNamer, n, state)

  /** Return a new `CompilerState` with a transformed AST */
  def map(f: Node => Node) = withNode(f(tree))
}
