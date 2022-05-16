from __future__ import annotations
from enum import Enum, auto
from typing import NamedTuple
from collections import Counter
from .lexicon import Lexicon


# enumeration of dependencies
vc = 'vc'
su = 'su'
obj1 = 'obj1'
obj2 = 'obj2'


def appl(left, right) -> Term:
    return left, right


def condia(left, right) -> Term:
    return left, right


# Term = Category | tuple[Term, Term] | tuple[str, Term]


class Category(Enum):
    INF0 = auto()
    INF1 = auto()
    INF1A = auto()
    IVR0 = auto()
    IVR1 = auto()
    IVR2 = auto()
    INF2 = auto()
    INF3 = auto()
    INF4 = auto()
    OBJ1A = auto()
    OBJ1I = auto()
    OBJ2 = auto()
    PREF1 = auto()
    PREF2 = auto()
    TE = auto()

    def to_lexicon(self) -> list[str]:
        return eval(f'Lexicon.{self.name.lower()}()')



# enumeration of canonical constants
iets = Category.OBJ1I
zal = Category.PREF2
haar = Category.OBJ1A
verzoeken = Category.INF3
hun = Category.OBJ2
laten = Category.IVR1
willen = Category.IVR0
ontmoeten = Category.INF1A
vertrekken = Category.INF0
proberen = Category.IVR2
te = Category.TE
beloven = Category.INF4
zeggen = Category.INF1
hij = Category.PREF1
dreigen = Category.INF2


# reading
AST = ...
Term = ...


class Sample(NamedTuple):
    ast:        AST
    sentence:   list[Category]
    term:       Term
    matchings:  list[tuple[Category, Category]]


def make_sample(lines: list[str]) -> Sample:
    # def eval_ast(line: str) -> AST: return eval(line)
    def eval_ast(line: str) -> AST: return line
    def eval_matchings(line: str) -> list[tuple[Category, Category]]: return eval(line)
    def eval_sentence(line: str) -> list[Category]: return list(map(eval, line.split()))
    def eval_semterm(line: str) -> Term: return eval(line)

    ast, _sentence, sem_term, matchings = lines
    sentence = eval_sentence(_sentence)
    counts = Counter(sentence)
    assert all([v == 1 for k, v in counts.items() if k != Category.TE])

    return Sample(eval_ast(ast), sentence, eval_semterm(sem_term), eval_matchings(matchings))


def load_samples(path: str) -> list[Sample]:
    with open(path, 'r') as in_file:
        data = list(map(lambda ln: ln.split('\n'), in_file.read().strip().split('\n\n')))
    return list(map(make_sample, data))


path = './prolog/sample.txt'
samples = load_samples(path)
# def generate(sequence: list[Category]) -> list[list[StringSample]]:
#     pass

if __name__ == '__main__':
    raise NotImplementedError
