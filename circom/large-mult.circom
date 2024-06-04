pragma circom 2.0.0;

template Multiplier(n) {
    signal input start;
    signal output out;

    signal int[n];

    int[0] <== start;
    for (var i=1; i<n; i++) {
        int[i] <== int[i-1] * (start + i);
    }

    out <== int[n-1];
}

component main = Multiplier(1000000);

