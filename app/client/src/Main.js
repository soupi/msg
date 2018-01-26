"use strict";

exports.scrollMessages = function () {
    var element = document.getElementById("messages");
    element.scrollTop = element.scrollHeight;
};
