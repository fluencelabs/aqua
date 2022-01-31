package aqua.model.transform.pre

import aqua.raw.ops.RawTag

trait PreTransform {
  def transform(op: RawTag.Tree): RawTag.Tree
}
