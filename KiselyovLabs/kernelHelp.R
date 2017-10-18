mc.kernel.indicator = function(r) +(abs(r) <= 1)

#RECT
mc.kernel.R = function(r) {
    0.5 * mc.kernel.indicator(r)
}

#TRIANGLE
mc.kernel.T = function(r) {
    (1 - abs(r)) * mc.kernel.indicator(r)
}

#QUARTIC
mc.kernel.Q = function(r) {
    (15 / 16) * (1 - r^2)^2 * mc.kernel.indicator(r)
}

#EPANECH
mc.kernel.E = function(r) {
    (3 / 4) * (1 - r^2) * mc.kernel.indicator(r)
}

#ALL
mc.kernels = list("R" = mc.kernel.R, "T" = mc.kernel.T, "Q" = mc.kernel.Q, "E" = mc.kernel.E)