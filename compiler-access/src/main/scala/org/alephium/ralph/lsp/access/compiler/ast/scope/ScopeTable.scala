package org.alephium.ralph.lsp.access.compiler.ast.scope

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.ast.scope.ScopeTable.{nearest, put}
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension

import scala.collection.mutable
import scala.reflect.ClassTag

object ScopeTable {

  /** Creates an empty scope table */
  def apply(): ScopeTable =
    ScopeTable(
      arguments = mutable.Map.empty,
      functions = mutable.Map.empty
    )

  /**
   * Find the nearest type definition for the given name.
   *
   * @param name  The name of the type.
   * @param index The index to search the closest for.
   * @param map   All type definitions.
   * @return The nearest type definition.
   * @tparam A The type of the element.
   */
  private def nearest[A <: Scope](name: String,
                                  index: SourceIndex,
                                  map: mutable.Map[String, Array[A]]): Option[A] =
    map
      .get(name)
      .flatMap {
        definitions =>
          nearest(
            definitions = definitions,
            index = index
          )
      }

  /**
   * Find the nearest type definition.
   *
   * @param index The index to search the closest for.
   * @param map   All type definitions.
   * @return The nearest type definition.
   * @tparam A The type of the element.
   */
  private def nearest[A <: Scope](definitions: Array[A],
                                  index: SourceIndex): Option[A] =
    definitions
      .filter(_.scope contains index.from)
      .sortBy(_.scope.index)
      .lastOption

  /**
   *
   * Inserts a new element into the map under the given name.
   *
   * @param name    The key under which the element is to be inserted.
   * @param typeDef The element to be inserted.
   * @param map     The mutable map to insert the element into.
   * @tparam A The type of the element.
   */
  private def put[A: ClassTag](name: String,
                               typeDef: A,
                               map: mutable.Map[String, Array[A]]) =
    map.get(name) match {
      case Some(scopes) =>
        map.put(name, scopes :+ typeDef)

      case None =>
        map.put(name, Array(typeDef))
    }
}

/**
 * Stores all declarations and their scope.
 *
 * @param arguments All arguments and their scopes.
 * @param functions All functions and their scopes.
 */
case class ScopeTable(private val arguments: mutable.Map[String, Array[Scope.Argument]],
                      private val functions: mutable.Map[String, Array[Scope.Function]]) {

  /** @inheritdoc [[ScopeTable.put]] */
  def putArgument(name: String,
                  namedScope: Scope.Argument): Option[Array[Scope.Argument]] =
    put(
      name = name,
      typeDef = namedScope,
      map = arguments
    )

  /** @inheritdoc [[ScopeTable.put]] */
  def putFunction(name: String,
                  namedScope: Scope.Function): Option[Array[Scope.Function]] =
    put(
      name = name,
      typeDef = namedScope,
      map = functions
    )

  /** @inheritdoc [[ScopeTable.nearest]] */
  def nearestArgument(name: String,
                      nearestToIndex: SourceIndex): Option[Scope.Argument] =
    nearest(
      name = name,
      index = nearestToIndex,
      map = arguments
    )

  /** @inheritdoc [[ScopeTable.nearest]] */
  def nearestFunction(name: String,
                      nearestToIndex: SourceIndex): Option[Scope.Function] =
    nearest(
      name = name,
      index = nearestToIndex,
      map = functions
    )
}
