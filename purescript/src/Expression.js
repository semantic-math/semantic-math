const unroll = require("Data.Functor.Mu").unroll;

exports.getId = function(v) {
    return unroll(v.value0.id);
};
