type t = {
  timestamps: Int64.t list;
  type_:      string  [@key "type"];
  when_:      Int64.t [@key "when"];
} [@@deriving of_yojson]
