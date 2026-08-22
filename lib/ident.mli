@@ portable

open! Core

type t = private string
type comparator_witness : value mod portable

include
  Identifiable.S with type t := t with type comparator_witness := comparator_witness
[@@modality portable]
