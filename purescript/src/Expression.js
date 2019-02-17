// const unroll = require("Data.Functor.Mu").unroll;

exports.getId = function(v) {
    return v.value0.id;
    // return unroll(v.value0.id); // roll and unroll are both the identity function
};
