import json
import time
from pathlib import Path
from random import uniform

source_dir = Path(__file__).parent


def random_point() -> tuple[float, float]:
    x = uniform(-90, 90)
    y = uniform(-180, 180)
    return x, y


num_of_points = 10 * 1_000_000  # 10 Million
filename = "data_10000000_flex.json"
points = []
print(f"Generating {num_of_points} random pair of points")
start = time.time()
for _ in range(num_of_points):
    x0, y0 = random_point()
    x1, y1 = random_point()
    points.append(dict(x0=x0, y0=y0, x1=x1, y1=y1))
with open(source_dir / filename, "w") as fp:
    fp.write(json.dumps({"pairs": points}, indent=4))
end = time.time()
print(f"Done. Tool {end - start:.2f} seconds")
