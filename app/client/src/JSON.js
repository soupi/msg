// taken from here: https://github.com/purescript/purescript-foreign/tree/master/examples/Util

"use strict";

exports.foreignValueImpl = function (left, right, str) {
  try {
    return right(JSON.parse(str));
  } catch (e) {
    return left(e.toString());
  }
};
