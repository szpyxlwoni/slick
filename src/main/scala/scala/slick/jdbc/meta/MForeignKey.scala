package scala.slick.jdbc.meta

import java.sql._
import scala.slick.jdbc.{JdbcBackend, ResultSetInvoker, Invoker}
import scala.slick.model.ForeignKeyAction

/** A wrapper for a row in the ResultSet returned by
  * DatabaseMetaData.getImportedKeys/getExportedKeys/getCrossReference(). */
case class MForeignKey(
  pkTable: MQName, pkColumn: String, fkTable: MQName, fkColumn: String,
  keySeq: Short, updateRule: ForeignKeyAction, deleteRule: ForeignKeyAction,
  fkName: Option[String], pkName: Option[String], deferrability: Short)

object MForeignKey {

  def getImportedKeys(table: MQName): Invoker[MForeignKey] =
    createInvoker(_.metaData.getImportedKeys(table.catalog_?, table.schema_?, table.name))

  def getExportedKeys(table: MQName): Invoker[MForeignKey] =
    createInvoker(_.metaData.getExportedKeys(table.catalog_?, table.schema_?, table.name))

  def getCrossReference(parentTable: MQName, foreignTable: MQName): Invoker[MForeignKey] =
    createInvoker(_.metaData.getCrossReference(
      parentTable.catalog_?, parentTable.schema_?, parentTable.name,
      foreignTable.catalog_?, foreignTable.schema_?, foreignTable.name))

  private[this] def createInvoker(f: JdbcBackend#Session => ResultSet) = ResultSetInvoker[MForeignKey](f) { r =>
    MForeignKey(MQName.from(r), r.<<, MQName.from(r), r.<<, r.<<, fkActionFor(r.<<), fkActionFor(r.<<), r.<<, r.<<, r.<<)
  }

  private[this] def fkActionFor(v: Short) = v match {
    case DatabaseMetaData.importedKeyNoAction => ForeignKeyAction.NoAction
    case DatabaseMetaData.importedKeyCascade => ForeignKeyAction.Cascade
    case DatabaseMetaData.importedKeySetNull => ForeignKeyAction.SetNull
    case DatabaseMetaData.importedKeySetDefault => ForeignKeyAction.SetDefault
    case DatabaseMetaData.importedKeyRestrict => ForeignKeyAction.Restrict
  }
}
