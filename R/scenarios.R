##################################
## DEFINE SIMULATION SCENARIOS  ##
##################################

# Non-null scenarios

# cluster randomisation, independence DE
scenarios_ClusInd = list(
list(name="A", rand_method="cluster", gee_method="ind", sample_sizes=c(790, 130, 54), ICC=0.1, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
list(name="B", rand_method="cluster", gee_method="ind", sample_sizes=c(814, 134, 56), ICC=0.1, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
list(name="C", rand_method="cluster", gee_method="ind", sample_sizes=c(832, 136, 56), ICC=0.1, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
list(name="D", rand_method="cluster", gee_method="ind", sample_sizes=c(852, 140, 58), ICC=0.1, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)),
list(name="E", rand_method="cluster", gee_method="ind", sample_sizes=c(798, 130, 54), ICC=0.5, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
list(name="F", rand_method="cluster", gee_method="ind", sample_sizes=c(918, 150, 62), ICC=0.5, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
list(name="G", rand_method="cluster", gee_method="ind", sample_sizes=c(1012, 166, 68), ICC=0.5, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
list(name="H", rand_method="cluster", gee_method="ind", sample_sizes=c(1110, 182, 74), ICC=0.5, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)),
list(name="I", rand_method="cluster", gee_method="ind", sample_sizes=c(808, 132, 54), ICC=0.9, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
list(name="J", rand_method="cluster", gee_method="ind", sample_sizes=c(1022, 168, 68), ICC=0.9, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
list(name="K", rand_method="cluster", gee_method="ind", sample_sizes=c(1192, 194, 80), ICC=0.9, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
list(name="L", rand_method="cluster", gee_method="ind", sample_sizes=c(1370, 224, 92), ICC=0.9, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)))

# cluster randomisation, exchangeable DE
scenarios_ClusExch = list(
  list(name="M", rand_method="cluster", gee_method="exch", sample_sizes=c(790, 130, 54), ICC=0.1, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="N", rand_method="cluster", gee_method="exch", sample_sizes=c(812, 132, 56), ICC=0.1, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="O", rand_method="cluster", gee_method="exch", sample_sizes=c(830, 136, 56), ICC=0.1, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="P", rand_method="cluster", gee_method="exch", sample_sizes=c(850, 140, 58), ICC=0.1, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="Q", rand_method="cluster", gee_method="exch", sample_sizes=c(794, 130, 54), ICC=0.5, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="R", rand_method="cluster", gee_method="exch", sample_sizes=c(886, 144, 60), ICC=0.5, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="S", rand_method="cluster", gee_method="exch", sample_sizes=c(972, 160, 66), ICC=0.5, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="T", rand_method="cluster", gee_method="exch", sample_sizes=c(1084, 178, 72), ICC=0.5, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="U", rand_method="cluster", gee_method="exch", sample_sizes=c(798, 130, 54), ICC=0.9, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="V", rand_method="cluster", gee_method="exch", sample_sizes=c(934, 152, 62), ICC=0.9, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="W", rand_method="cluster", gee_method="exch", sample_sizes=c(1078, 176, 72), ICC=0.9, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="X", rand_method="cluster", gee_method="exch", sample_sizes=c(1290, 210, 86), ICC=0.9, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)))

# individual randomisation, independence DE
scenarios_IndInd = list(
  list(name="Y", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.1, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="Z", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.1, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AA", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.1, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AB", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.1, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AC", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.5, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AD", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.5, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AE", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.5, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AF", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.5, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AG", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.9, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AH", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.9, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AI", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.9, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AJ", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.9, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)))

# individual randomisation, exchangeable DE
scenarios_IndExch = list(
  list(name="AK", rand_method="ind", gee_method="exch", sample_sizes=c(786, 128, 52), ICC=0.1, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AL", rand_method="ind", gee_method="exch", sample_sizes=c(784, 128, 52), ICC=0.1, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AM", rand_method="ind", gee_method="exch", sample_sizes=c(782, 128, 52), ICC=0.1, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AN", rand_method="ind", gee_method="exch", sample_sizes=c(780, 128, 52), ICC=0.1, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AO", rand_method="ind", gee_method="exch", sample_sizes=c(780, 128, 52), ICC=0.5, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AP", rand_method="ind", gee_method="exch", sample_sizes=c(708, 116, 48), ICC=0.5, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AQ", rand_method="ind", gee_method="exch", sample_sizes=c(662, 108, 44), ICC=0.5, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AR", rand_method="ind", gee_method="exch", sample_sizes=c(618, 102, 42), ICC=0.5, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AS", rand_method="ind", gee_method="exch", sample_sizes=c(700, 114, 48), ICC=0.9, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AT", rand_method="ind", gee_method="exch", sample_sizes=c(326, 54, 22), ICC=0.9, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AU", rand_method="ind", gee_method="exch", sample_sizes=c(230, 38, 16), ICC=0.9, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AV", rand_method="ind", gee_method="exch", sample_sizes=c(176, 30, 12), ICC=0.9, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)))

# opposite randomisation, independence DE
scenarios_BalInd = list(
  list(name="AW", rand_method="opp", gee_method="ind", sample_sizes=c(784, 128, 52), ICC=0.1, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AX", rand_method="opp", gee_method="ind", sample_sizes=c(760, 124, 50), ICC=0.1, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AY", rand_method="opp", gee_method="ind", sample_sizes=c(742, 122, 50), ICC=0.1, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="AZ", rand_method="opp", gee_method="ind", sample_sizes=c(722, 118, 48), ICC=0.1, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BA", rand_method="opp", gee_method="ind", sample_sizes=c(776, 128, 52), ICC=0.5, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BB", rand_method="opp", gee_method="ind", sample_sizes=c(656, 108, 44), ICC=0.5, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BC", rand_method="opp", gee_method="ind", sample_sizes=c(562, 92, 38), ICC=0.5, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BD", rand_method="opp", gee_method="ind", sample_sizes=c(464, 76, 32), ICC=0.5, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BE", rand_method="opp", gee_method="ind", sample_sizes=c(766, 126, 52), ICC=0.9, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BF", rand_method="opp", gee_method="ind", sample_sizes=c(552, 90, 38), ICC=0.9, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BG", rand_method="opp", gee_method="ind", sample_sizes=c(382, 64, 26), ICC=0.9, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BH", rand_method="opp", gee_method="ind", sample_sizes=c(204, 34, 14), ICC=0.9, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)))

# opposite randomisation, exchangeable DE
scenarios_BalExch = list(
  list(name="BI", rand_method="opp", gee_method="exch", sample_sizes=c(784, 128, 52), ICC=0.1, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BJ", rand_method="opp", gee_method="exch", sample_sizes=c(758, 124, 48), ICC=0.1, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BK", rand_method="opp", gee_method="exch", sample_sizes=c(740, 122, 50), ICC=0.1, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BL", rand_method="opp", gee_method="exch", sample_sizes=c(722, 118, 48), ICC=0.1, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BM", rand_method="opp", gee_method="exch", sample_sizes=c(764, 126, 52), ICC=0.5, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BN", rand_method="opp", gee_method="exch", sample_sizes=c(590, 96, 40), ICC=0.5, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BO", rand_method="opp", gee_method="exch", sample_sizes=c(502, 82, 34), ICC=0.5, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BP", rand_method="opp", gee_method="exch", sample_sizes=c(432, 72, 30), ICC=0.5, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BQ", rand_method="opp", gee_method="exch", sample_sizes=c(622, 102, 42), ICC=0.9, ppair=0.015, b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BR", rand_method="opp", gee_method="exch", sample_sizes=c(198, 32, 14), ICC=0.9, ppair=0.2,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BS", rand_method="opp", gee_method="exch", sample_sizes=c(128, 22, 10), ICC=0.9, ppair=0.4,   b0=0, b1=c(0.2, 0.5, 0.8)),
  list(name="BT", rand_method="opp", gee_method="exch", sample_sizes=c(94, 16, 8), ICC=0.9, ppair=0.7,   b0=0, b1=c(0.2, 0.5, 0.8)))

# null scenarios

# cluster randomisation, independence DE
scenarios_null_ClusInd = list(
  list(name="A", rand_method="cluster", gee_method="ind", sample_sizes=c(790, 130, 54), ICC=0.1, ppair=0.015, b0=0, b1=c(0,0,0)),
  list(name="B", rand_method="cluster", gee_method="ind", sample_sizes=c(814, 134, 56), ICC=0.1, ppair=0.2,   b0=0, b1=c(0,0,0)),
  list(name="C", rand_method="cluster", gee_method="ind", sample_sizes=c(832, 136, 56), ICC=0.1, ppair=0.4,   b0=0, b1=c(0,0,0)),
  list(name="D", rand_method="cluster", gee_method="ind", sample_sizes=c(852, 140, 58), ICC=0.1, ppair=0.7,   b0=0, b1=c(0,0,0)),
  list(name="E", rand_method="cluster", gee_method="ind", sample_sizes=c(798, 130, 54), ICC=0.5, ppair=0.015, b0=0, b1=c(0,0,0)),
  list(name="F", rand_method="cluster", gee_method="ind", sample_sizes=c(918, 150, 62), ICC=0.5, ppair=0.2,   b0=0, b1=c(0,0,0)),
  list(name="G", rand_method="cluster", gee_method="ind", sample_sizes=c(1012, 166, 68), ICC=0.5, ppair=0.4,  b0=0, b1=c(0,0,0)),
  list(name="H", rand_method="cluster", gee_method="ind", sample_sizes=c(1110, 182, 74), ICC=0.5, ppair=0.7,  b0=0, b1=c(0,0,0)),
  list(name="I", rand_method="cluster", gee_method="ind", sample_sizes=c(808, 132, 54), ICC=0.9, ppair=0.015, b0=0, b1=c(0,0,0)),
  list(name="J", rand_method="cluster", gee_method="ind", sample_sizes=c(1022, 168, 68), ICC=0.9, ppair=0.2,  b0=0, b1=c(0,0,0)),
  list(name="K", rand_method="cluster", gee_method="ind", sample_sizes=c(1192, 194, 80), ICC=0.9, ppair=0.4,  b0=0, b1=c(0,0,0)),
  list(name="L", rand_method="cluster", gee_method="ind", sample_sizes=c(1370, 224, 92), ICC=0.9, ppair=0.7,  b0=0, b1=c(0,0,0)))

# cluster randomisation, exchangeable DE
scenarios_null_ClusExch = list(
  list(name="M", rand_method="cluster", gee_method="exch", sample_sizes=c(790, 130, 54), ICC=0.1, ppair=0.015, b0=0, b1=c(0, 0, 0)),
  list(name="N", rand_method="cluster", gee_method="exch", sample_sizes=c(812, 132, 56), ICC=0.1, ppair=0.2,   b0=0, b1=c(0, 0, 0)),
  list(name="O", rand_method="cluster", gee_method="exch", sample_sizes=c(830, 136, 56), ICC=0.1, ppair=0.4,   b0=0, b1=c(0, 0, 0)),
  list(name="P", rand_method="cluster", gee_method="exch", sample_sizes=c(850, 140, 58), ICC=0.1, ppair=0.7,   b0=0, b1=c(0, 0, 0)),
  list(name="Q", rand_method="cluster", gee_method="exch", sample_sizes=c(794, 130, 54), ICC=0.5, ppair=0.015, b0=0, b1=c(0, 0, 0)),
  list(name="R", rand_method="cluster", gee_method="exch", sample_sizes=c(886, 144, 60), ICC=0.5, ppair=0.2,   b0=0, b1=c(0, 0, 0)),
  list(name="S", rand_method="cluster", gee_method="exch", sample_sizes=c(972, 160, 66), ICC=0.5, ppair=0.4,   b0=0, b1=c(0, 0, 0)),
  list(name="T", rand_method="cluster", gee_method="exch", sample_sizes=c(1084, 178, 72), ICC=0.5, ppair=0.7,   b0=0, b1=c(0, 0, 0)),
  list(name="U", rand_method="cluster", gee_method="exch", sample_sizes=c(798, 130, 54), ICC=0.9, ppair=0.015, b0=0, b1=c(0, 0, 0)),
  list(name="V", rand_method="cluster", gee_method="exch", sample_sizes=c(934, 152, 62), ICC=0.9, ppair=0.2,   b0=0, b1=c(0, 0, 0)),
  list(name="W", rand_method="cluster", gee_method="exch", sample_sizes=c(1078, 176, 72), ICC=0.9, ppair=0.4,   b0=0, b1=c(0, 0, 0)),
  list(name="X", rand_method="cluster", gee_method="exch", sample_sizes=c(1290, 210, 86), ICC=0.9, ppair=0.7,   b0=0, b1=c(0, 0, 0)))

# individual randomisation, independence DE
scenarios_null_IndInd = list(
  list(name="Y", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.1, ppair=0.015, b0=0, b1=c(0,0,0)),
  list(name="Z", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.1, ppair=0.2,   b0=0, b1=c(0,0,0)),
  list(name="AA", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.1, ppair=0.4,   b0=0, b1=c(0,0,0)),
  list(name="AB", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.1, ppair=0.7,   b0=0, b1=c(0,0,0)),
  list(name="AC", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.5, ppair=0.015, b0=0, b1=c(0,0,0)),
  list(name="AD", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.5, ppair=0.2,   b0=0, b1=c(0,0,0)),
  list(name="AE", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.5, ppair=0.4,   b0=0, b1=c(0,0,0)),
  list(name="AF", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.5, ppair=0.7,   b0=0, b1=c(0,0,0)),
  list(name="AG", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.9, ppair=0.015, b0=0, b1=c(0,0,0)),
  list(name="AH", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.9, ppair=0.2,   b0=0, b1=c(0,0,0)),
  list(name="AI", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.9, ppair=0.4,   b0=0, b1=c(0,0,0)),
  list(name="AJ", rand_method="ind", gee_method="ind", sample_sizes=c(786, 128, 52), ICC=0.9, ppair=0.7,   b0=0, b1=c(0,0,0)))

# individual randomisation, exchangeable DE
scenarios_null_IndExch = list(
  list(name="AK", rand_method="ind", gee_method="exch", sample_sizes=c(786, 128, 52), ICC=0.1, ppair=0.015, b0=0, b1=c(0, 0, 0)),
  list(name="AL", rand_method="ind", gee_method="exch", sample_sizes=c(784, 128, 52), ICC=0.1, ppair=0.2,   b0=0, b1=c(0, 0, 0)),
  list(name="AM", rand_method="ind", gee_method="exch", sample_sizes=c(782, 128, 52), ICC=0.1, ppair=0.4,   b0=0, b1=c(0, 0, 0)),
  list(name="AN", rand_method="ind", gee_method="exch", sample_sizes=c(780, 128, 52), ICC=0.1, ppair=0.7,   b0=0, b1=c(0, 0, 0)),
  list(name="AO", rand_method="ind", gee_method="exch", sample_sizes=c(780, 128, 52), ICC=0.5, ppair=0.015, b0=0, b1=c(0, 0, 0)),
  list(name="AP", rand_method="ind", gee_method="exch", sample_sizes=c(708, 116, 48), ICC=0.5, ppair=0.2,   b0=0, b1=c(0, 0, 0)),
  list(name="AQ", rand_method="ind", gee_method="exch", sample_sizes=c(662, 108, 44), ICC=0.5, ppair=0.4,   b0=0, b1=c(0, 0, 0)),
  list(name="AR", rand_method="ind", gee_method="exch", sample_sizes=c(618, 102, 42), ICC=0.5, ppair=0.7,   b0=0, b1=c(0, 0, 0)),
  list(name="AS", rand_method="ind", gee_method="exch", sample_sizes=c(700, 114, 48), ICC=0.9, ppair=0.015, b0=0, b1=c(0, 0, 0)),
  list(name="AT", rand_method="ind", gee_method="exch", sample_sizes=c(326, 54, 22), ICC=0.9, ppair=0.2,   b0=0, b1=c(0, 0, 0)),
  list(name="AU", rand_method="ind", gee_method="exch", sample_sizes=c(230, 38, 16), ICC=0.9, ppair=0.4,   b0=0, b1=c(0, 0, 0)),
  list(name="AV", rand_method="ind", gee_method="exch", sample_sizes=c(176, 30, 12), ICC=0.9, ppair=0.7,   b0=0, b1=c(0, 0, 0)))

# opposite randomisation, independence DE
scenarios_null_BalInd = list(
  list(name="AW", rand_method="opp", gee_method="ind", sample_sizes=c(784, 128, 52), ICC=0.1, ppair=0.015, b0=0, b1=c(0, 0, 0)),
  list(name="AX", rand_method="opp", gee_method="ind", sample_sizes=c(760, 124, 50), ICC=0.1, ppair=0.2,   b0=0, b1=c(0, 0, 0)),
  list(name="AY", rand_method="opp", gee_method="ind", sample_sizes=c(742, 122, 50), ICC=0.1, ppair=0.4,   b0=0, b1=c(0, 0, 0)),
  list(name="AZ", rand_method="opp", gee_method="ind", sample_sizes=c(722, 118, 48), ICC=0.1, ppair=0.7,   b0=0, b1=c(0, 0, 0)),
  list(name="BA", rand_method="opp", gee_method="ind", sample_sizes=c(776, 128, 52), ICC=0.5, ppair=0.015, b0=0, b1=c(0, 0, 0)),
  list(name="BB", rand_method="opp", gee_method="ind", sample_sizes=c(656, 108, 44), ICC=0.5, ppair=0.2,   b0=0, b1=c(0, 0, 0)),
  list(name="BC", rand_method="opp", gee_method="ind", sample_sizes=c(562, 92, 38), ICC=0.5, ppair=0.4,   b0=0, b1=c(0, 0, 0)),
  list(name="BD", rand_method="opp", gee_method="ind", sample_sizes=c(464, 76, 32), ICC=0.5, ppair=0.7,   b0=0, b1=c(0, 0, 0)),
  list(name="BE", rand_method="opp", gee_method="ind", sample_sizes=c(766, 126, 52), ICC=0.9, ppair=0.015, b0=0, b1=c(0, 0, 0)),
  list(name="BF", rand_method="opp", gee_method="ind", sample_sizes=c(552, 90, 38), ICC=0.9, ppair=0.2,   b0=0, b1=c(0, 0, 0)),
  list(name="BG", rand_method="opp", gee_method="ind", sample_sizes=c(382, 64, 26), ICC=0.9, ppair=0.4,   b0=0, b1=c(0, 0, 0)),
  list(name="BH", rand_method="opp", gee_method="ind", sample_sizes=c(204, 34, 14), ICC=0.9, ppair=0.7,   b0=0, b1=c(0, 0, 0)))

# opposite randomisation, exchangeable DE
scenarios_null_BalExch = list(
  list(name="BI", rand_method="opp", gee_method="exch", sample_sizes=c(784, 128, 52), ICC=0.1, ppair=0.015, b0=0, b1=c(0, 0, 0)),
  list(name="BJ", rand_method="opp", gee_method="exch", sample_sizes=c(758, 124, 48), ICC=0.1, ppair=0.2,   b0=0, b1=c(0, 0, 0)),
  list(name="BK", rand_method="opp", gee_method="exch", sample_sizes=c(740, 122, 50), ICC=0.1, ppair=0.4,   b0=0, b1=c(0, 0, 0)),
  list(name="BL", rand_method="opp", gee_method="exch", sample_sizes=c(722, 118, 48), ICC=0.1, ppair=0.7,   b0=0, b1=c(0, 0, 0)),
  list(name="BM", rand_method="opp", gee_method="exch", sample_sizes=c(764, 126, 52), ICC=0.5, ppair=0.015, b0=0, b1=c(0, 0, 0)),
  list(name="BN", rand_method="opp", gee_method="exch", sample_sizes=c(590, 96, 40), ICC=0.5, ppair=0.2,   b0=0, b1=c(0, 0, 0)),
  list(name="BO", rand_method="opp", gee_method="exch", sample_sizes=c(502, 82, 34), ICC=0.5, ppair=0.4,   b0=0, b1=c(0, 0, 0)),
  list(name="BP", rand_method="opp", gee_method="exch", sample_sizes=c(432, 72, 30), ICC=0.5, ppair=0.7,   b0=0, b1=c(0, 0, 0)),
  list(name="BQ", rand_method="opp", gee_method="exch", sample_sizes=c(622, 102, 42), ICC=0.9, ppair=0.015, b0=0, b1=c(0, 0, 0)),
  list(name="BR", rand_method="opp", gee_method="exch", sample_sizes=c(198, 32, 14), ICC=0.9, ppair=0.2,   b0=0, b1=c(0, 0, 0)),
  list(name="BS", rand_method="opp", gee_method="exch", sample_sizes=c(128, 22, 10), ICC=0.9, ppair=0.4,   b0=0, b1=c(0, 0, 0)),
  list(name="BT", rand_method="opp", gee_method="exch", sample_sizes=c(94, 16, 8), ICC=0.9, ppair=0.7,   b0=0, b1=c(0, 0, 0)))

# results from null models
# identify the three different scenarios for each ICC/ppair combination via the sample size
id_null_scenarios = function(res) {
  my_rand = res$rand_method[1]
  my_gee = res$gee_method[1]
  if (my_rand == "cluster" & my_gee == "ind") {
    res = res %>% 
      mutate(
        n_es = case_when(
          ICC == 0.1 & ppair == 0.015 & nobs == 790 ~ 0.2,
          ICC == 0.1 & ppair == 0.015 & nobs == 130 ~ 0.5,
          ICC == 0.1 & ppair == 0.015 & nobs == 54  ~ 0.8,
          ICC == 0.1 & ppair == 0.2 & nobs == 814 ~ 0.2,
          ICC == 0.1 & ppair == 0.2 & nobs == 134 ~ 0.5,
          ICC == 0.1 & ppair == 0.2 & nobs == 56  ~ 0.8,
          ICC == 0.1 & ppair == 0.4 & nobs == 832 ~ 0.2,
          ICC == 0.1 & ppair == 0.4 & nobs == 136 ~ 0.5,
          ICC == 0.1 & ppair == 0.4 & nobs == 56  ~ 0.8,
          ICC == 0.1 & ppair == 0.7 & nobs == 852 ~ 0.2,
          ICC == 0.1 & ppair == 0.7 & nobs == 140 ~ 0.5,
          ICC == 0.1 & ppair == 0.7 & nobs == 58  ~ 0.8,
          ICC == 0.5 & ppair == 0.015 & nobs == 798 ~ 0.2,
          ICC == 0.5 & ppair == 0.015 & nobs == 130 ~ 0.5,
          ICC == 0.5 & ppair == 0.015 & nobs == 54 ~ 0.8,
          ICC == 0.5 & ppair == 0.2 & nobs == 918 ~ 0.2,
          ICC == 0.5 & ppair == 0.2 & nobs == 150 ~ 0.5,
          ICC == 0.5 & ppair == 0.2 & nobs == 62  ~ 0.8,
          ICC == 0.5 & ppair == 0.4 & nobs == 1012 ~ 0.2,
          ICC == 0.5 & ppair == 0.4 & nobs == 166 ~ 0.5,
          ICC == 0.5 & ppair == 0.4 & nobs == 68  ~ 0.8,
          ICC == 0.5 & ppair == 0.7 & nobs == 1110 ~ 0.2,
          ICC == 0.5 & ppair == 0.7 & nobs == 182 ~ 0.5,
          ICC == 0.5 & ppair == 0.7 & nobs == 74  ~ 0.8,
          ICC == 0.9 & ppair == 0.015 & nobs == 808 ~ 0.2,
          ICC == 0.9 & ppair == 0.015 & nobs == 132  ~ 0.5,
          ICC == 0.9 & ppair == 0.015 & nobs == 54 ~ 0.8,
          ICC == 0.9 & ppair == 0.2 & nobs == 1022 ~ 0.2,
          ICC == 0.9 & ppair == 0.2 & nobs == 168 ~ 0.5,
          ICC == 0.9 & ppair == 0.2 & nobs == 68  ~ 0.8,
          ICC == 0.9 & ppair == 0.4 & nobs == 1192 ~ 0.2,
          ICC == 0.9 & ppair == 0.4 & nobs == 194 ~ 0.5,
          ICC == 0.9 & ppair == 0.4 & nobs == 80  ~ 0.8,
          ICC == 0.9 & ppair == 0.7 & nobs == 1370 ~ 0.2,
          ICC == 0.9 & ppair == 0.7 & nobs == 224 ~ 0.5,
          ICC == 0.9 & ppair == 0.7 & nobs == 92  ~ 0.8
        )
      )
  } else if (my_rand == "cluster" & my_gee == "exch") {
    res = res %>% 
      mutate(
        n_es = case_when(
          ICC == 0.1 & ppair == 0.015 & nobs == 790 ~ 0.2,
          ICC == 0.1 & ppair == 0.015 & nobs == 130 ~ 0.5,
          ICC == 0.1 & ppair == 0.015 & nobs == 54  ~ 0.8,
          ICC == 0.1 & ppair == 0.2 & nobs == 812 ~ 0.2,
          ICC == 0.1 & ppair == 0.2 & nobs == 132 ~ 0.5,
          ICC == 0.1 & ppair == 0.2 & nobs == 56  ~ 0.8,
          ICC == 0.1 & ppair == 0.4 & nobs == 830 ~ 0.2,
          ICC == 0.1 & ppair == 0.4 & nobs == 136 ~ 0.5,
          ICC == 0.1 & ppair == 0.4 & nobs == 56  ~ 0.8,
          ICC == 0.1 & ppair == 0.7 & nobs == 850 ~ 0.2,
          ICC == 0.1 & ppair == 0.7 & nobs == 140 ~ 0.5,
          ICC == 0.1 & ppair == 0.7 & nobs == 58  ~ 0.8,
          ICC == 0.5 & ppair == 0.015 & nobs == 794 ~ 0.2,
          ICC == 0.5 & ppair == 0.015 & nobs == 130 ~ 0.5,
          ICC == 0.5 & ppair == 0.015 & nobs == 54 ~ 0.8,
          ICC == 0.5 & ppair == 0.2 & nobs == 886 ~ 0.2,
          ICC == 0.5 & ppair == 0.2 & nobs == 144 ~ 0.5,
          ICC == 0.5 & ppair == 0.2 & nobs == 60  ~ 0.8,
          ICC == 0.5 & ppair == 0.4 & nobs == 972 ~ 0.2,
          ICC == 0.5 & ppair == 0.4 & nobs == 160 ~ 0.5,
          ICC == 0.5 & ppair == 0.4 & nobs == 66  ~ 0.8,
          ICC == 0.5 & ppair == 0.7 & nobs == 1084 ~ 0.2,
          ICC == 0.5 & ppair == 0.7 & nobs == 178 ~ 0.5,
          ICC == 0.5 & ppair == 0.7 & nobs == 72  ~ 0.8,
          ICC == 0.9 & ppair == 0.015 & nobs == 798 ~ 0.2,
          ICC == 0.9 & ppair == 0.015 & nobs == 130  ~ 0.5,
          ICC == 0.9 & ppair == 0.015 & nobs == 54 ~ 0.8,
          ICC == 0.9 & ppair == 0.2 & nobs == 934 ~ 0.2,
          ICC == 0.9 & ppair == 0.2 & nobs == 152 ~ 0.5,
          ICC == 0.9 & ppair == 0.2 & nobs == 62  ~ 0.8,
          ICC == 0.9 & ppair == 0.4 & nobs == 1078 ~ 0.2,
          ICC == 0.9 & ppair == 0.4 & nobs == 176 ~ 0.5,
          ICC == 0.9 & ppair == 0.4 & nobs == 72  ~ 0.8,
          ICC == 0.9 & ppair == 0.7 & nobs == 1290 ~ 0.2,
          ICC == 0.9 & ppair == 0.7 & nobs == 210 ~ 0.5,
          ICC == 0.9 & ppair == 0.7 & nobs == 86  ~ 0.8
        )
      )
  } else if (my_rand == "ind" & my_gee == "ind") {
    res = res %>% 
      mutate(
        n_es = case_when(
          ICC == 0.1 & ppair == 0.015 & nobs == 786 ~ 0.2,
          ICC == 0.1 & ppair == 0.015 & nobs == 128 ~ 0.5,
          ICC == 0.1 & ppair == 0.015 & nobs == 52  ~ 0.8,
          ICC == 0.1 & ppair == 0.2 & nobs == 786 ~ 0.2,
          ICC == 0.1 & ppair == 0.2 & nobs == 128 ~ 0.5,
          ICC == 0.1 & ppair == 0.2 & nobs == 52  ~ 0.8,
          ICC == 0.1 & ppair == 0.4 & nobs == 786 ~ 0.2,
          ICC == 0.1 & ppair == 0.4 & nobs == 128 ~ 0.5,
          ICC == 0.1 & ppair == 0.4 & nobs == 52  ~ 0.8,
          ICC == 0.1 & ppair == 0.7 & nobs == 786 ~ 0.2,
          ICC == 0.1 & ppair == 0.7 & nobs == 128 ~ 0.5,
          ICC == 0.1 & ppair == 0.7 & nobs == 52  ~ 0.8,
          ICC == 0.5 & ppair == 0.015 & nobs == 786 ~ 0.2,
          ICC == 0.5 & ppair == 0.015 & nobs == 128 ~ 0.5,
          ICC == 0.5 & ppair == 0.015 & nobs == 52 ~ 0.8,
          ICC == 0.5 & ppair == 0.2 & nobs == 786 ~ 0.2,
          ICC == 0.5 & ppair == 0.2 & nobs == 128 ~ 0.5,
          ICC == 0.5 & ppair == 0.2 & nobs == 52  ~ 0.8,
          ICC == 0.5 & ppair == 0.4 & nobs == 786 ~ 0.2,
          ICC == 0.5 & ppair == 0.4 & nobs == 128 ~ 0.5,
          ICC == 0.5 & ppair == 0.4 & nobs == 52  ~ 0.8,
          ICC == 0.5 & ppair == 0.7 & nobs == 786 ~ 0.2,
          ICC == 0.5 & ppair == 0.7 & nobs == 128 ~ 0.5,
          ICC == 0.5 & ppair == 0.7 & nobs == 52  ~ 0.8,
          ICC == 0.9 & ppair == 0.015 & nobs == 786 ~ 0.2,
          ICC == 0.9 & ppair == 0.015 & nobs == 128  ~ 0.5,
          ICC == 0.9 & ppair == 0.015 & nobs == 52 ~ 0.8,
          ICC == 0.9 & ppair == 0.2 & nobs == 786 ~ 0.2,
          ICC == 0.9 & ppair == 0.2 & nobs == 128 ~ 0.5,
          ICC == 0.9 & ppair == 0.2 & nobs == 52  ~ 0.8,
          ICC == 0.9 & ppair == 0.4 & nobs == 786 ~ 0.2,
          ICC == 0.9 & ppair == 0.4 & nobs == 128 ~ 0.5,
          ICC == 0.9 & ppair == 0.4 & nobs == 52  ~ 0.8,
          ICC == 0.9 & ppair == 0.7 & nobs == 786 ~ 0.2,
          ICC == 0.9 & ppair == 0.7 & nobs == 128 ~ 0.5,
          ICC == 0.9 & ppair == 0.7 & nobs == 52  ~ 0.8
        )
      )
  } else if (my_rand == "ind" & my_gee == "exch") {
    res = res %>% 
      mutate(
        n_es = case_when(
          ICC == 0.1 & ppair == 0.015 & nobs == 786 ~ 0.2,
          ICC == 0.1 & ppair == 0.015 & nobs == 128 ~ 0.5,
          ICC == 0.1 & ppair == 0.015 & nobs == 52  ~ 0.8,
          ICC == 0.1 & ppair == 0.2 & nobs == 784 ~ 0.2,
          ICC == 0.1 & ppair == 0.2 & nobs == 128 ~ 0.5,
          ICC == 0.1 & ppair == 0.2 & nobs == 52  ~ 0.8,
          ICC == 0.1 & ppair == 0.4 & nobs == 782 ~ 0.2,
          ICC == 0.1 & ppair == 0.4 & nobs == 128 ~ 0.5,
          ICC == 0.1 & ppair == 0.4 & nobs == 52  ~ 0.8,
          ICC == 0.1 & ppair == 0.7 & nobs == 780 ~ 0.2,
          ICC == 0.1 & ppair == 0.7 & nobs == 128 ~ 0.5,
          ICC == 0.1 & ppair == 0.7 & nobs == 52  ~ 0.8,
          ICC == 0.5 & ppair == 0.015 & nobs == 780 ~ 0.2,
          ICC == 0.5 & ppair == 0.015 & nobs == 128 ~ 0.5,
          ICC == 0.5 & ppair == 0.015 & nobs == 52 ~ 0.8,
          ICC == 0.5 & ppair == 0.2 & nobs == 708 ~ 0.2,
          ICC == 0.5 & ppair == 0.2 & nobs == 116 ~ 0.5,
          ICC == 0.5 & ppair == 0.2 & nobs == 48  ~ 0.8,
          ICC == 0.5 & ppair == 0.4 & nobs == 662 ~ 0.2,
          ICC == 0.5 & ppair == 0.4 & nobs == 108 ~ 0.5,
          ICC == 0.5 & ppair == 0.4 & nobs == 44  ~ 0.8,
          ICC == 0.5 & ppair == 0.7 & nobs == 618 ~ 0.2,
          ICC == 0.5 & ppair == 0.7 & nobs == 102 ~ 0.5,
          ICC == 0.5 & ppair == 0.7 & nobs == 42  ~ 0.8,
          ICC == 0.9 & ppair == 0.015 & nobs == 700 ~ 0.2,
          ICC == 0.9 & ppair == 0.015 & nobs == 114  ~ 0.5,
          ICC == 0.9 & ppair == 0.015 & nobs == 48 ~ 0.8,
          ICC == 0.9 & ppair == 0.2 & nobs == 326 ~ 0.2,
          ICC == 0.9 & ppair == 0.2 & nobs == 54 ~ 0.5,
          ICC == 0.9 & ppair == 0.2 & nobs == 22  ~ 0.8,
          ICC == 0.9 & ppair == 0.4 & nobs == 230 ~ 0.2,
          ICC == 0.9 & ppair == 0.4 & nobs == 38 ~ 0.5,
          ICC == 0.9 & ppair == 0.4 & nobs == 16  ~ 0.8,
          ICC == 0.9 & ppair == 0.7 & nobs == 176 ~ 0.2,
          ICC == 0.9 & ppair == 0.7 & nobs == 30 ~ 0.5,
          ICC == 0.9 & ppair == 0.7 & nobs == 12  ~ 0.8
        )
      )
  } else if (my_rand == "opp" & my_gee == "ind") {
    res = res %>% 
      mutate(
        n_es = case_when(
          ICC == 0.1 & ppair == 0.015 & nobs == 784 ~ 0.2,
          ICC == 0.1 & ppair == 0.015 & nobs == 128 ~ 0.5,
          ICC == 0.1 & ppair == 0.015 & nobs == 52  ~ 0.8,
          ICC == 0.1 & ppair == 0.2 & nobs == 760 ~ 0.2,
          ICC == 0.1 & ppair == 0.2 & nobs == 124 ~ 0.5,
          ICC == 0.1 & ppair == 0.2 & nobs == 50  ~ 0.8,
          ICC == 0.1 & ppair == 0.4 & nobs == 742 ~ 0.2,
          ICC == 0.1 & ppair == 0.4 & nobs == 122 ~ 0.5,
          ICC == 0.1 & ppair == 0.4 & nobs == 50  ~ 0.8,
          ICC == 0.1 & ppair == 0.7 & nobs == 722 ~ 0.2,
          ICC == 0.1 & ppair == 0.7 & nobs == 118 ~ 0.5,
          ICC == 0.1 & ppair == 0.7 & nobs == 48  ~ 0.8,
          ICC == 0.5 & ppair == 0.015 & nobs == 776 ~ 0.2,
          ICC == 0.5 & ppair == 0.015 & nobs == 128 ~ 0.5,
          ICC == 0.5 & ppair == 0.015 & nobs == 52 ~ 0.8,
          ICC == 0.5 & ppair == 0.2 & nobs == 656 ~ 0.2,
          ICC == 0.5 & ppair == 0.2 & nobs == 108 ~ 0.5,
          ICC == 0.5 & ppair == 0.2 & nobs == 44  ~ 0.8,
          ICC == 0.5 & ppair == 0.4 & nobs == 562 ~ 0.2,
          ICC == 0.5 & ppair == 0.4 & nobs == 92 ~ 0.5,
          ICC == 0.5 & ppair == 0.4 & nobs == 38  ~ 0.8,
          ICC == 0.5 & ppair == 0.7 & nobs == 464 ~ 0.2,
          ICC == 0.5 & ppair == 0.7 & nobs == 76 ~ 0.5,
          ICC == 0.5 & ppair == 0.7 & nobs == 32  ~ 0.8,
          ICC == 0.9 & ppair == 0.015 & nobs == 766 ~ 0.2,
          ICC == 0.9 & ppair == 0.015 & nobs == 126  ~ 0.5,
          ICC == 0.9 & ppair == 0.015 & nobs == 52 ~ 0.8,
          ICC == 0.9 & ppair == 0.2 & nobs == 552 ~ 0.2,
          ICC == 0.9 & ppair == 0.2 & nobs == 90 ~ 0.5,
          ICC == 0.9 & ppair == 0.2 & nobs == 38  ~ 0.8,
          ICC == 0.9 & ppair == 0.4 & nobs == 382 ~ 0.2,
          ICC == 0.9 & ppair == 0.4 & nobs == 64 ~ 0.5,
          ICC == 0.9 & ppair == 0.4 & nobs == 26  ~ 0.8,
          ICC == 0.9 & ppair == 0.7 & nobs == 204 ~ 0.2,
          ICC == 0.9 & ppair == 0.7 & nobs == 34 ~ 0.5,
          ICC == 0.9 & ppair == 0.7 & nobs == 14  ~ 0.8
        )
      )
  } else if (my_rand == "opp" & my_gee == "exch") {
    res = res %>% 
      mutate(
        n_es = case_when(
          ICC == 0.1 & ppair == 0.015 & nobs == 784 ~ 0.2,
          ICC == 0.1 & ppair == 0.015 & nobs == 128 ~ 0.5,
          ICC == 0.1 & ppair == 0.015 & nobs == 52  ~ 0.8,
          ICC == 0.1 & ppair == 0.2 & nobs == 758 ~ 0.2,
          ICC == 0.1 & ppair == 0.2 & nobs == 124 ~ 0.5,
          ICC == 0.1 & ppair == 0.2 & nobs == 48  ~ 0.8,
          ICC == 0.1 & ppair == 0.4 & nobs == 740 ~ 0.2,
          ICC == 0.1 & ppair == 0.4 & nobs == 122 ~ 0.5,
          ICC == 0.1 & ppair == 0.4 & nobs == 50  ~ 0.8,
          ICC == 0.1 & ppair == 0.7 & nobs == 722 ~ 0.2,
          ICC == 0.1 & ppair == 0.7 & nobs == 118 ~ 0.5,
          ICC == 0.1 & ppair == 0.7 & nobs == 48  ~ 0.8,
          ICC == 0.5 & ppair == 0.015 & nobs == 764 ~ 0.2,
          ICC == 0.5 & ppair == 0.015 & nobs == 126 ~ 0.5,
          ICC == 0.5 & ppair == 0.015 & nobs == 52 ~ 0.8,
          ICC == 0.5 & ppair == 0.2 & nobs == 590 ~ 0.2,
          ICC == 0.5 & ppair == 0.2 & nobs == 96 ~ 0.5,
          ICC == 0.5 & ppair == 0.2 & nobs == 40  ~ 0.8,
          ICC == 0.5 & ppair == 0.4 & nobs == 502 ~ 0.2,
          ICC == 0.5 & ppair == 0.4 & nobs == 82 ~ 0.5,
          ICC == 0.5 & ppair == 0.4 & nobs == 34  ~ 0.8,
          ICC == 0.5 & ppair == 0.7 & nobs == 432 ~ 0.2,
          ICC == 0.5 & ppair == 0.7 & nobs == 72 ~ 0.5,
          ICC == 0.5 & ppair == 0.7 & nobs == 30  ~ 0.8,
          ICC == 0.9 & ppair == 0.015 & nobs == 622 ~ 0.2,
          ICC == 0.9 & ppair == 0.015 & nobs == 102  ~ 0.5,
          ICC == 0.9 & ppair == 0.015 & nobs == 42 ~ 0.8,
          ICC == 0.9 & ppair == 0.2 & nobs == 198 ~ 0.2,
          ICC == 0.9 & ppair == 0.2 & nobs == 32 ~ 0.5,
          ICC == 0.9 & ppair == 0.2 & nobs == 14  ~ 0.8,
          ICC == 0.9 & ppair == 0.4 & nobs == 128 ~ 0.2,
          ICC == 0.9 & ppair == 0.4 & nobs == 22 ~ 0.5,
          ICC == 0.9 & ppair == 0.4 & nobs == 10  ~ 0.8,
          ICC == 0.9 & ppair == 0.7 & nobs == 94 ~ 0.2,
          ICC == 0.9 & ppair == 0.7 & nobs == 16 ~ 0.5,
          ICC == 0.9 & ppair == 0.7 & nobs == 8  ~ 0.8
        )
      )
  }
  return(res)
}