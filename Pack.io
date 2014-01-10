Pack := Object clone

Object || := method(self)
nil    || := method(o, o)

Object fitLeft := method(length,  s, asString exclusiveSlice(0, length)  alignLeft(length, s || " "))
Object fitRight := method(length, s, asString exclusiveSlice(0, length) alignRight(length, s || " "))

Number asStream := method(kind,
  if(kind isNil, kind = "int32")
  Sequence withStruct(list(kind, self))
)

Number fromStream := method(kind, stream,
  if(kind isNil, kind = "int32")
  stream, stream asStruct(list(kind, "x")) x
)

// f = 32 bit float
// d = 64 bit float
//
// c =  8 bit int (signed)
// s = 16 bit int (signed)
// l = 32 bit int (signed)
// q = 64 bit int (signed)
//
// C =  8 bit int (unsigned)
// S = 16 bit int (unsigned)
// L = 32 bit int (unsigned)
// Q = 64 bit int (unsigned)
//
// A = string, space padded
List pack := method(formatString,
  if(size <= 0, return "")

  match := formatString findRegex("^(?<directive>[a-zA-Z])(?<count>[0-9]*) *(?<rest>.*)$")
  count := lazySlot(match at("count") asNumber)

  match at("directive") switch(
    "f", first asNumber asStream("float32"),
    "d", first asNumber asStream("float64"),

    "c", first asNumber asStream("int8"),
    "s", first asNumber asStream("int16"),
    "l", first asNumber asStream("int32"),
    "q", first asNumber asStream("int64"),

    "C", first asNumber asStream("uint8"),
    "S", first asNumber asStream("uint16"),
    "L", first asNumber asStream("uint32"),
    "Q", first asNumber asStream("uint64"),

    "A", first fitLeft(count),
    Exception raise("not a valid format character")
  ) .. (rest pack(match at("rest")))
)

Sequence unpack := method(formatString,
  if(size <= 0, return list)

  match := formatString findRegex("^(?<directive>[a-zA-Z])(?<count>[0-9]*) *(?<rest>.*)$")
  count := lazySlot(match at("count") asNumber)
  s     := 0

  list(match at("directive") switch(
    "f", (s = 4; Number fromStream("float32", self)),
    "d", (s = 8; Number fromStream("float64", self)),

    "c", (s = 1; Number fromStream("int8", self)),
    "s", (s = 2; Number fromStream("int16", self)),
    "l", (s = 4; Number fromStream("int32", self)),
    "q", (s = 8; Number fromStream("int64", self)),

    "C", (s = 1; Number fromStream("uint8", self)),
    "S", (s = 2; Number fromStream("uint16", self)),
    "L", (s = 4; Number fromStream("uint32", self)),
    "Q", (s = 8; Number fromStream("uint64", self)),

    "A", exclusiveSlice(0, s = count),

    Exception raise("not a valid format character")
  )) appendSeq(exclusiveSlice(s) unpack(match at("rest")))
)
