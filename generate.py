from src.populate.generate import dump_to_json, load_concrete_samples
import random


def main(ipath: str = './src/prolog/sample.txt', opath: str = './populated.json1', n: int = 10):
    return dump_to_json(load_concrete_samples(ipath, n), opath)


if __name__ == '__main__':
    random.seed(42)
    main()
