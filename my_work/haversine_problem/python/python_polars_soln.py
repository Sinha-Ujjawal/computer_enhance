import time

import polars as pl

start_time = time.time()
df = (
    pl.read_json("data_10000000_flex.json")
    .lazy()
    .explode("pairs")
    .unnest("pairs")
    .cast({"x0": pl.Float32, "y0": pl.Float32, "x1": pl.Float32, "y1": pl.Float32})
    .collect()
)
mid_time = time.time()


def haversine_of_degrees(
    *,
    df: pl.DataFrame,
    x0_col: str,
    y0_col: str,
    x1_col: str,
    y1_col: str,
    radius: float,
) -> pl.DataFrame:
    # Convert input columns to radians for trigonometric calculation
    # X corresponds to Longitude, Y corresponds to Latitude
    y0 = pl.col(y0_col).radians()
    y1 = pl.col(y1_col).radians()
    dx = (pl.col(x1_col) - pl.col(x0_col)).radians()
    dy = (pl.col(y1_col) - pl.col(y0_col)).radians()

    # Formula components
    root_term = (dy / 2).sin() ** 2 + y0.cos() * y1.cos() * (dx / 2).sin() ** 2

    # Calculate final distance
    distance_expr = 2 * radius * root_term.sqrt().arcsin()

    return df.with_columns(haversine_dist=distance_expr)


df_with_haversine = haversine_of_degrees(
    df=df,
    x0_col="x0",
    y0_col="y0",
    x1_col="x1",
    y1_col="y1",
    radius=6371,
)

count = len(df_with_haversine)
average = df_with_haversine["haversine_dist"].mean()

end_time = time.time()


print("Result: " + str(average))
print("Input = " + str(mid_time - start_time) + " seconds")
print("Math = " + str(end_time - mid_time) + " seconds")
print("Total = " + str(end_time - start_time) + " seconds")
print("Throughput = " + str(count / (end_time - start_time)) + " haversines/second")
