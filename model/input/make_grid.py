import csv

with open("grid_vary_base_weights.csv", "w") as csv_file:
    writer = csv.writer(csv_file, delimiter=",")
    for beta in [2]:
        for fairness_weight in [0.1]:
            for comm_weight in [i/5 for i in range(11)]:
                for beta_params in [{"a": 1, "b": 1}, {"a": 1, "b": 2}]:
                    writer.writerow(
                        [
                            beta,
                            fairness_weight,
                            comm_weight,
                            "vary_base_weights",
                            beta_params["a"],
                            beta_params["b"]
                        ]
                    )