(** In its current state, the goal of this module is to provide an alternative
    [String Notation] for [string], which supports escape characters such as
    <<\n>>, <<\t>> and others. *)

From Coq Require Export Ascii String.
From Coq Require Import List Byte.

Import ListNotations.

#[local]
Infix "<$>" := option_map (at level 50).

Fixpoint string_of_list_byte_fmt (i : list byte) : option string :=
  match i with
  | x5c :: x30 :: rst => String (ascii_of_byte x00) <$> string_of_list_byte_fmt rst
  | x5c :: x6e :: rst => String (ascii_of_byte x0a) <$> string_of_list_byte_fmt rst
  | x5c :: x72 :: rst => String (ascii_of_byte x0d) <$> string_of_list_byte_fmt rst
  | x5c :: x74 :: rst => String (ascii_of_byte x09) <$> string_of_list_byte_fmt rst
  | x5c :: x5c :: rst => String (ascii_of_byte x5c) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x30 :: x30 :: rst => String (ascii_of_byte x00) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x30 :: x31 :: rst => String (ascii_of_byte x01) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x30 :: x32 :: rst => String (ascii_of_byte x02) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x30 :: x33 :: rst => String (ascii_of_byte x03) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x30 :: x34 :: rst => String (ascii_of_byte x04) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x30 :: x35 :: rst => String (ascii_of_byte x05) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x30 :: x36 :: rst => String (ascii_of_byte x06) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x30 :: x37 :: rst => String (ascii_of_byte x07) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x30 :: x38 :: rst => String (ascii_of_byte x08) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x30 :: x39 :: rst => String (ascii_of_byte x09) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x30 :: x61 :: rst => String (ascii_of_byte x0a) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x30 :: x62 :: rst => String (ascii_of_byte x0b) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x30 :: x63 :: rst => String (ascii_of_byte x0c) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x30 :: x64 :: rst => String (ascii_of_byte x0d) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x30 :: x65 :: rst => String (ascii_of_byte x0e) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x30 :: x66 :: rst => String (ascii_of_byte x0f) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x31 :: x30 :: rst => String (ascii_of_byte x10) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x31 :: x31 :: rst => String (ascii_of_byte x11) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x31 :: x32 :: rst => String (ascii_of_byte x12) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x31 :: x33 :: rst => String (ascii_of_byte x13) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x31 :: x34 :: rst => String (ascii_of_byte x14) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x31 :: x35 :: rst => String (ascii_of_byte x15) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x31 :: x36 :: rst => String (ascii_of_byte x16) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x31 :: x37 :: rst => String (ascii_of_byte x17) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x31 :: x38 :: rst => String (ascii_of_byte x18) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x31 :: x39 :: rst => String (ascii_of_byte x19) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x31 :: x61 :: rst => String (ascii_of_byte x1a) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x31 :: x62 :: rst => String (ascii_of_byte x1b) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x31 :: x63 :: rst => String (ascii_of_byte x1c) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x31 :: x64 :: rst => String (ascii_of_byte x1d) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x31 :: x65 :: rst => String (ascii_of_byte x1e) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x31 :: x66 :: rst => String (ascii_of_byte x1f) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x32 :: x30 :: rst => String (ascii_of_byte x20) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x32 :: x31 :: rst => String (ascii_of_byte x21) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x32 :: x32 :: rst => String (ascii_of_byte x22) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x32 :: x33 :: rst => String (ascii_of_byte x23) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x32 :: x34 :: rst => String (ascii_of_byte x24) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x32 :: x35 :: rst => String (ascii_of_byte x25) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x32 :: x36 :: rst => String (ascii_of_byte x26) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x32 :: x37 :: rst => String (ascii_of_byte x27) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x32 :: x38 :: rst => String (ascii_of_byte x28) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x32 :: x39 :: rst => String (ascii_of_byte x29) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x32 :: x61 :: rst => String (ascii_of_byte x2a) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x32 :: x62 :: rst => String (ascii_of_byte x2b) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x32 :: x63 :: rst => String (ascii_of_byte x2c) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x32 :: x64 :: rst => String (ascii_of_byte x2d) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x32 :: x65 :: rst => String (ascii_of_byte x2e) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x32 :: x66 :: rst => String (ascii_of_byte x2f) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x33 :: x30 :: rst => String (ascii_of_byte x30) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x33 :: x31 :: rst => String (ascii_of_byte x31) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x33 :: x32 :: rst => String (ascii_of_byte x32) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x33 :: x33 :: rst => String (ascii_of_byte x33) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x33 :: x34 :: rst => String (ascii_of_byte x34) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x33 :: x35 :: rst => String (ascii_of_byte x35) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x33 :: x36 :: rst => String (ascii_of_byte x36) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x33 :: x37 :: rst => String (ascii_of_byte x37) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x33 :: x38 :: rst => String (ascii_of_byte x38) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x33 :: x39 :: rst => String (ascii_of_byte x39) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x33 :: x61 :: rst => String (ascii_of_byte x3a) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x33 :: x62 :: rst => String (ascii_of_byte x3b) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x33 :: x63 :: rst => String (ascii_of_byte x3c) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x33 :: x64 :: rst => String (ascii_of_byte x3d) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x33 :: x65 :: rst => String (ascii_of_byte x3e) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x33 :: x66 :: rst => String (ascii_of_byte x3f) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x34 :: x30 :: rst => String (ascii_of_byte x40) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x34 :: x31 :: rst => String (ascii_of_byte x41) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x34 :: x32 :: rst => String (ascii_of_byte x42) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x34 :: x33 :: rst => String (ascii_of_byte x43) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x34 :: x34 :: rst => String (ascii_of_byte x44) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x34 :: x35 :: rst => String (ascii_of_byte x45) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x34 :: x36 :: rst => String (ascii_of_byte x46) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x34 :: x37 :: rst => String (ascii_of_byte x47) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x34 :: x38 :: rst => String (ascii_of_byte x48) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x34 :: x39 :: rst => String (ascii_of_byte x49) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x34 :: x61 :: rst => String (ascii_of_byte x4a) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x34 :: x62 :: rst => String (ascii_of_byte x4b) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x34 :: x63 :: rst => String (ascii_of_byte x4c) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x34 :: x64 :: rst => String (ascii_of_byte x4d) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x34 :: x65 :: rst => String (ascii_of_byte x4e) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x34 :: x66 :: rst => String (ascii_of_byte x4f) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x35 :: x30 :: rst => String (ascii_of_byte x50) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x35 :: x31 :: rst => String (ascii_of_byte x51) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x35 :: x32 :: rst => String (ascii_of_byte x52) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x35 :: x33 :: rst => String (ascii_of_byte x53) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x35 :: x34 :: rst => String (ascii_of_byte x54) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x35 :: x35 :: rst => String (ascii_of_byte x55) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x35 :: x36 :: rst => String (ascii_of_byte x56) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x35 :: x37 :: rst => String (ascii_of_byte x57) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x35 :: x38 :: rst => String (ascii_of_byte x58) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x35 :: x39 :: rst => String (ascii_of_byte x59) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x35 :: x61 :: rst => String (ascii_of_byte x5a) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x35 :: x62 :: rst => String (ascii_of_byte x5b) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x35 :: x63 :: rst => String (ascii_of_byte x5c) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x35 :: x64 :: rst => String (ascii_of_byte x5d) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x35 :: x65 :: rst => String (ascii_of_byte x5e) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x35 :: x66 :: rst => String (ascii_of_byte x5f) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x36 :: x30 :: rst => String (ascii_of_byte x60) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x36 :: x31 :: rst => String (ascii_of_byte x61) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x36 :: x32 :: rst => String (ascii_of_byte x62) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x36 :: x33 :: rst => String (ascii_of_byte x63) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x36 :: x34 :: rst => String (ascii_of_byte x64) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x36 :: x35 :: rst => String (ascii_of_byte x65) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x36 :: x36 :: rst => String (ascii_of_byte x66) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x36 :: x37 :: rst => String (ascii_of_byte x67) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x36 :: x38 :: rst => String (ascii_of_byte x68) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x36 :: x39 :: rst => String (ascii_of_byte x69) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x36 :: x61 :: rst => String (ascii_of_byte x6a) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x36 :: x62 :: rst => String (ascii_of_byte x6b) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x36 :: x63 :: rst => String (ascii_of_byte x6c) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x36 :: x64 :: rst => String (ascii_of_byte x6d) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x36 :: x65 :: rst => String (ascii_of_byte x6e) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x36 :: x66 :: rst => String (ascii_of_byte x6f) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x37 :: x30 :: rst => String (ascii_of_byte x70) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x37 :: x31 :: rst => String (ascii_of_byte x71) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x37 :: x32 :: rst => String (ascii_of_byte x72) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x37 :: x33 :: rst => String (ascii_of_byte x73) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x37 :: x34 :: rst => String (ascii_of_byte x74) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x37 :: x35 :: rst => String (ascii_of_byte x75) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x37 :: x36 :: rst => String (ascii_of_byte x76) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x37 :: x37 :: rst => String (ascii_of_byte x77) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x37 :: x38 :: rst => String (ascii_of_byte x78) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x37 :: x39 :: rst => String (ascii_of_byte x79) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x37 :: x61 :: rst => String (ascii_of_byte x7a) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x37 :: x62 :: rst => String (ascii_of_byte x7b) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x37 :: x63 :: rst => String (ascii_of_byte x7c) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x37 :: x64 :: rst => String (ascii_of_byte x7d) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x37 :: x65 :: rst => String (ascii_of_byte x7e) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x37 :: x66 :: rst => String (ascii_of_byte x7f) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x38 :: x30 :: rst => String (ascii_of_byte x80) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x38 :: x31 :: rst => String (ascii_of_byte x81) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x38 :: x32 :: rst => String (ascii_of_byte x82) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x38 :: x33 :: rst => String (ascii_of_byte x83) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x38 :: x34 :: rst => String (ascii_of_byte x84) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x38 :: x35 :: rst => String (ascii_of_byte x85) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x38 :: x36 :: rst => String (ascii_of_byte x86) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x38 :: x37 :: rst => String (ascii_of_byte x87) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x38 :: x38 :: rst => String (ascii_of_byte x88) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x38 :: x39 :: rst => String (ascii_of_byte x89) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x38 :: x61 :: rst => String (ascii_of_byte x8a) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x38 :: x62 :: rst => String (ascii_of_byte x8b) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x38 :: x63 :: rst => String (ascii_of_byte x8c) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x38 :: x64 :: rst => String (ascii_of_byte x8d) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x38 :: x65 :: rst => String (ascii_of_byte x8e) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x38 :: x66 :: rst => String (ascii_of_byte x8f) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x39 :: x30 :: rst => String (ascii_of_byte x90) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x39 :: x31 :: rst => String (ascii_of_byte x91) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x39 :: x32 :: rst => String (ascii_of_byte x92) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x39 :: x33 :: rst => String (ascii_of_byte x93) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x39 :: x34 :: rst => String (ascii_of_byte x94) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x39 :: x35 :: rst => String (ascii_of_byte x95) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x39 :: x36 :: rst => String (ascii_of_byte x96) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x39 :: x37 :: rst => String (ascii_of_byte x97) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x39 :: x38 :: rst => String (ascii_of_byte x98) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x39 :: x39 :: rst => String (ascii_of_byte x99) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x39 :: x61 :: rst => String (ascii_of_byte x9a) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x39 :: x62 :: rst => String (ascii_of_byte x9b) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x39 :: x63 :: rst => String (ascii_of_byte x9c) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x39 :: x64 :: rst => String (ascii_of_byte x9d) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x39 :: x65 :: rst => String (ascii_of_byte x9e) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x39 :: x66 :: rst => String (ascii_of_byte x9f) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x61 :: x30 :: rst => String (ascii_of_byte xa0) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x61 :: x31 :: rst => String (ascii_of_byte xa1) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x61 :: x32 :: rst => String (ascii_of_byte xa2) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x61 :: x33 :: rst => String (ascii_of_byte xa3) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x61 :: x34 :: rst => String (ascii_of_byte xa4) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x61 :: x35 :: rst => String (ascii_of_byte xa5) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x61 :: x36 :: rst => String (ascii_of_byte xa6) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x61 :: x37 :: rst => String (ascii_of_byte xa7) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x61 :: x38 :: rst => String (ascii_of_byte xa8) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x61 :: x39 :: rst => String (ascii_of_byte xa9) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x61 :: x61 :: rst => String (ascii_of_byte xaa) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x61 :: x62 :: rst => String (ascii_of_byte xab) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x61 :: x63 :: rst => String (ascii_of_byte xac) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x61 :: x64 :: rst => String (ascii_of_byte xad) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x61 :: x65 :: rst => String (ascii_of_byte xae) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x61 :: x66 :: rst => String (ascii_of_byte xaf) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x62 :: x30 :: rst => String (ascii_of_byte xb0) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x62 :: x31 :: rst => String (ascii_of_byte xb1) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x62 :: x32 :: rst => String (ascii_of_byte xb2) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x62 :: x33 :: rst => String (ascii_of_byte xb3) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x62 :: x34 :: rst => String (ascii_of_byte xb4) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x62 :: x35 :: rst => String (ascii_of_byte xb5) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x62 :: x36 :: rst => String (ascii_of_byte xb6) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x62 :: x37 :: rst => String (ascii_of_byte xb7) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x62 :: x38 :: rst => String (ascii_of_byte xb8) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x62 :: x39 :: rst => String (ascii_of_byte xb9) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x62 :: x61 :: rst => String (ascii_of_byte xba) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x62 :: x62 :: rst => String (ascii_of_byte xbb) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x62 :: x63 :: rst => String (ascii_of_byte xbc) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x62 :: x64 :: rst => String (ascii_of_byte xbd) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x62 :: x65 :: rst => String (ascii_of_byte xbe) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x62 :: x66 :: rst => String (ascii_of_byte xbf) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x63 :: x30 :: rst => String (ascii_of_byte xc0) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x63 :: x31 :: rst => String (ascii_of_byte xc1) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x63 :: x32 :: rst => String (ascii_of_byte xc2) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x63 :: x33 :: rst => String (ascii_of_byte xc3) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x63 :: x34 :: rst => String (ascii_of_byte xc4) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x63 :: x35 :: rst => String (ascii_of_byte xc5) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x63 :: x36 :: rst => String (ascii_of_byte xc6) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x63 :: x37 :: rst => String (ascii_of_byte xc7) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x63 :: x38 :: rst => String (ascii_of_byte xc8) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x63 :: x39 :: rst => String (ascii_of_byte xc9) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x63 :: x61 :: rst => String (ascii_of_byte xca) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x63 :: x62 :: rst => String (ascii_of_byte xcb) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x63 :: x63 :: rst => String (ascii_of_byte xcc) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x63 :: x64 :: rst => String (ascii_of_byte xcd) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x63 :: x65 :: rst => String (ascii_of_byte xce) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x63 :: x66 :: rst => String (ascii_of_byte xcf) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x64 :: x30 :: rst => String (ascii_of_byte xd0) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x64 :: x31 :: rst => String (ascii_of_byte xd1) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x64 :: x32 :: rst => String (ascii_of_byte xd2) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x64 :: x33 :: rst => String (ascii_of_byte xd3) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x64 :: x34 :: rst => String (ascii_of_byte xd4) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x64 :: x35 :: rst => String (ascii_of_byte xd5) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x64 :: x36 :: rst => String (ascii_of_byte xd6) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x64 :: x37 :: rst => String (ascii_of_byte xd7) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x64 :: x38 :: rst => String (ascii_of_byte xd8) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x64 :: x39 :: rst => String (ascii_of_byte xd9) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x64 :: x61 :: rst => String (ascii_of_byte xda) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x64 :: x62 :: rst => String (ascii_of_byte xdb) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x64 :: x63 :: rst => String (ascii_of_byte xdc) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x64 :: x64 :: rst => String (ascii_of_byte xdd) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x64 :: x65 :: rst => String (ascii_of_byte xde) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x64 :: x66 :: rst => String (ascii_of_byte xdf) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x65 :: x30 :: rst => String (ascii_of_byte xe0) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x65 :: x31 :: rst => String (ascii_of_byte xe1) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x65 :: x32 :: rst => String (ascii_of_byte xe2) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x65 :: x33 :: rst => String (ascii_of_byte xe3) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x65 :: x34 :: rst => String (ascii_of_byte xe4) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x65 :: x35 :: rst => String (ascii_of_byte xe5) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x65 :: x36 :: rst => String (ascii_of_byte xe6) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x65 :: x37 :: rst => String (ascii_of_byte xe7) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x65 :: x38 :: rst => String (ascii_of_byte xe8) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x65 :: x39 :: rst => String (ascii_of_byte xe9) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x65 :: x61 :: rst => String (ascii_of_byte xea) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x65 :: x62 :: rst => String (ascii_of_byte xeb) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x65 :: x63 :: rst => String (ascii_of_byte xec) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x65 :: x64 :: rst => String (ascii_of_byte xed) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x65 :: x65 :: rst => String (ascii_of_byte xee) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x65 :: x66 :: rst => String (ascii_of_byte xef) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x66 :: x30 :: rst => String (ascii_of_byte xf0) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x66 :: x31 :: rst => String (ascii_of_byte xf1) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x66 :: x32 :: rst => String (ascii_of_byte xf2) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x66 :: x33 :: rst => String (ascii_of_byte xf3) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x66 :: x34 :: rst => String (ascii_of_byte xf4) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x66 :: x35 :: rst => String (ascii_of_byte xf5) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x66 :: x36 :: rst => String (ascii_of_byte xf6) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x66 :: x37 :: rst => String (ascii_of_byte xf7) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x66 :: x38 :: rst => String (ascii_of_byte xf8) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x66 :: x39 :: rst => String (ascii_of_byte xf9) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x66 :: x61 :: rst => String (ascii_of_byte xfa) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x66 :: x62 :: rst => String (ascii_of_byte xfb) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x66 :: x63 :: rst => String (ascii_of_byte xfc) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x66 :: x64 :: rst => String (ascii_of_byte xfd) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x66 :: x65 :: rst => String (ascii_of_byte xfe) <$> string_of_list_byte_fmt rst
  | x5c :: x78 :: x66 :: x66 :: rst => String (ascii_of_byte xff) <$> string_of_list_byte_fmt rst
  | x5c :: _     :: _ => None
  | x     :: rst          => String (ascii_of_byte x) <$> string_of_list_byte_fmt rst
  | [] => Some EmptyString
  end.

Fixpoint list_byte_of_string (x : string) : list byte :=
  match x with
  | String x rst => byte_of_ascii x :: list_byte_of_string rst
  | EmptyString => []
  end.

String Notation string string_of_list_byte_fmt list_byte_of_string
  : string_scope.
