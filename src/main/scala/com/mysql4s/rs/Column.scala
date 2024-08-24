package com.mysql4s.rs

import com.mysql4s.bindings.enumerations.enum_field_types

import scala.scalanative.unsafe.CVoidPtr

private[mysql4s] case class Column(name: String,
                                   index: Int,
                                   typ: enum_field_types,
                                   isNull: Boolean = false,
                                   error: Boolean = false,
                                   length: Int = 0,
                                   ptr: CVoidPtr = null,
                                   precision: Int = 0,
                                   decimals: Int = 0)