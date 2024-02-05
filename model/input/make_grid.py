import csv

with open("grid_vary_beta.csv", "w") as csv_file:
    writer = csv.writer(csv_file, delimiter=",")
    for beta in [1, 2, 4, 8]:
        for comm_weight in [i/10 for i in range(21)]:
                    writer.writerow(
                        [
                            beta,
                            comm_weight,
                            "vary_beta"
                        ]
                    )

with open("grid_vary_base_weights.csv", "w") as csv_file:
    writer = csv.writer(csv_file, delimiter=",")
    for beta in [2]:
        for fairness_weight in [0.1]:
            for comm_weight in [i/5 for i in range(11)]:
                    writer.writerow(
                        [
                            beta,
                            fairness_weight,
                            comm_weight,
                            "vary_base_weights"
                        ]
                    )

with open("grid_vary_base_weights_fairness.csv", "w") as csv_file:
    writer = csv.writer(csv_file, delimiter=",")
    for beta in [2]:
        for fairness_weight in [i/20 for i in range(11)]:
            for comm_weight in [0]:
                    writer.writerow(
                        [
                            beta,
                            fairness_weight,
                            comm_weight,
                            "vary_base_weights_fairness"
                        ]
                    )

with open("grid_fairness_comm.csv", "w") as csv_file:
    writer = csv.writer(csv_file, delimiter=",")
    for fairness_weight in [i/20 for i in range(11)]:
        for comm_weight in [i/10 for i in range(11)]:
                    writer.writerow(
                        [
                            beta,
                            fairness_weight,
                            comm_weight,
                            "fairness_comm"
                        ]
                    )