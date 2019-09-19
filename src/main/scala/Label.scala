package eidos
package id

trait Label[A] {
  def label: String
}

object Label {
  private val defaultLabel: Label[Nothing] = new Label[Nothing] {
    val label = ""
  }

  private[id] def default[A]: Label[A] = defaultLabel.asInstanceOf[Label[A]]

  private[id] sealed trait LabelDefinitionConflict

  private def nameOf[A](implicit m: Manifest[A]) = {
    val typeName = m.toString
    typeName.substring(0, typeName.lastIndexOf(".type"))
  }

  trait MakeLabel { self =>
    // See eidos.id.Format.UUID for an explanation of this
    // format: off
    final def `"In Eidos, you can only extend one of MakeLabel or CustomLabel"`
        : LabelDefinitionConflict = null

    implicit final val l: Label[this.type] = new Label[this.type] {
      val label: String = nameOf[self.type]
    }
  }

  trait CustomLabel {
    final def `"In Eidos, you can only extend one of MakeLabel or CustomLabel"`
        : LabelDefinitionConflict = null
    // format: on
    def label: String

    private val customLabel = label

    implicit final val l: Label[this.type] = new Label[this.type] {
      val label = customLabel
    }
  }
}
