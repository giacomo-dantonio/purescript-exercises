"use strict";

// module Exercises.Chapter10

exports.union = function(lhs) {
    return function (rhs) {
	var result = {};

	// side effects (bad guy :)
	var update = function(rec) {
	    for (var k in rec) {
		if (rec.hasOwnProperty(k)) {
		    result[k] = rec[k];
		}
	    }
	};

	update(lhs);
	update(rhs);
	
	return result;
    };
};


exports.eqHRec = function (lhs) {
    return function(rhs) {
	for (var k in lhs) {
	    if (lhs.hasOwnProperty(k) && 
		(!rhs.hasOwnProperty(k) || lhs[k] !== rhs[k])) {
		return false;
	    }
	}
	return true;
    };
};


exports.mapHRec2 = function(f, rec) {
    var mapped = {};
    for (var k in rec) {
        if (rec.hasOwnProperty(k)) {
            mapped[k] = f(k)(rec[k]);
        }
    }
    return mapped;
};


exports.confirm = function(msg) {
    return function() {
	window.confirm(msg);
	return {};
    };
};


exports.removeItem = function(key) {
    return function() {
        return window.localStorage.removeItem(key);
    }
};
