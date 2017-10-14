#RECT
mc.kernel.R = function(dist) {
    dist[dist < -1] = 0
    dist[dist > 1] = 0
    dist[dist != 0] = 0.5
    return(dist)
}

#TRIANGLE
mc.kernel.T = function(dist) {
    dist[dist < -1] = 0
    dist[dist > 1] = 0
    dist[dist != 0] = -abs(dist[dist != 0]) + 1
    return(dist)
}

#QUADRATIC
mc.kernel.Q = function(dist) {
    dist[dist < -1] = 0
    dist[dist > 1] = 0
    dist[dist != 0] = -(dist[dist != 0] ^ 2) + 1
    return(dist)
}