from __future__ import annotations
from enum import Enum, auto
from typing import NamedTuple, TypeVar

Choices = TypeVar('Choices')


########################################################################################################################
# Categories
########################################################################################################################
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

    def to_lexicon(self) -> Choices: return eval(f'Lexicon.{self.name.lower()}()')  # todo
    def __repr__(self) -> str: return self.name

# Canonical Constants
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


########################################################################################################################
# Abstract Syntax Trees
########################################################################################################################
class AST:
    def __matmul__(self, other) -> RuleTree: return RuleTree(self, other)
    def __len__(self) -> int: raise NotImplementedError
    def __repr__(self) -> str: raise NotImplementedError


class Rule(AST, Enum):
    r0 = auto()
    r1 = auto()
    f0 = auto()
    f1 = auto()
    f2 = auto()
    d0 = auto()
    d1 = auto()
    d2 = auto()
    d3 = auto()
    d4 = auto()
    d5 = auto()
    d6 = auto()
    d7 = auto()
    d8 = auto()
    d9 = auto()
    d10 = auto()
    d11 = auto()
    d12 = auto()
    g0 = auto()
    g1 = auto()
    h0 = auto()
    h1 = auto()
    x0 = auto()
    x1 = auto()
    x2 = auto()
    x3 = auto()
    x4 = auto()
    x5 = auto()
    x6 = auto()
    x7 = auto()
    x8 = auto()
    x9 = auto()

    def __repr__(self) -> str: return self.name
    def __len__(self) -> int: return 0


class RuleTree(AST):
    def __init__(self, left: AST, right: AST): self.left = left; self.right = right
    def __repr__(self) -> str: return f'({repr(self.left)}, {repr(self.right)})'
    def __len__(self) -> int: return 1 + max(len(self.left), len(self.right))


# Rule shortcuts
r0 = Rule.r0
r1 = Rule.r1
f0 = Rule.f0
f1 = Rule.f1
f2 = Rule.f2
d0 = Rule.d0
d1 = Rule.d1
d2 = Rule.d2
d3 = Rule.d3
d4 = Rule.d4
d5 = Rule.d5
d6 = Rule.d6
d7 = Rule.d7
d8 = Rule.d8
d9 = Rule.d9
d10 = Rule.d10
d11 = Rule.d11
d12 = Rule.d12
g0 = Rule.g0
g1 = Rule.g1
h0 = Rule.h0
h1 = Rule.h1
x0 = Rule.x0
x1 = Rule.x1
x2 = Rule.x2
x3 = Rule.x3
x4 = Rule.x4
x5 = Rule.x5
x6 = Rule.x6
x7 = Rule.x7
x8 = Rule.x8
x9 = Rule.x9


########################################################################################################################
# Semantic Terms
########################################################################################################################
class Term:
    def __len__(self) -> int: raise NotImplementedError
    def __repr__(self) -> str: raise NotImplementedError


class Application(Term):
    def __init__(self, function: Term | Category, argument: Term | Category):
        self.function = function
        self.argument = argument

    def __len__(self) -> int: return 1 + max(len(self.function), len(self.argument))
    def __repr__(self) -> str: return f'appl({repr(self.function)}, {repr(self.argument)})'


class DiaElim(Term):
    def __init__(self, diamond: str, body: Term | Category):
        self.diamond = diamond
        self.body = body

    def __len__(self) -> int: return len(self.body)
    def __repr__(self) -> str: return f'condia({self.diamond}, {repr(self.body)})'


vc = 'vc'
su = 'su'
obj1 = 'obj1'
obj2 = 'obj2'
...


def appl(left, right) -> Application:
    return Application(left, right)


def condia(left, right) -> DiaElim:
    return DiaElim(left, right)


########################################################################################################################
# Samples
########################################################################################################################
class Sample(NamedTuple):
    ast:        AST
    sentence:   list[Category]
    term:       Term
    matchings:  list[tuple[Category, Category]]


def make_sample(lines: list[str]) -> Sample:
    def eval_ast(line: str) -> AST: return eval(line)
    def eval_matchings(line: str) -> list[tuple[Category, Category]]: return eval(line)
    def eval_sentence(line: str) -> list[Category]: return list(map(eval, line.split()))
    def eval_semterm(line: str) -> Term: return eval(line)
    return Sample(*[f(x) for f, x in zip([eval_ast, eval_sentence, eval_semterm, eval_matchings], lines)])


def load_samples(path: str) -> list[Sample]:
    with open(path, 'r') as in_file:
        data = list(map(lambda ln: ln.split('\n'), in_file.read().strip().split('\n\n')))
    return list(map(make_sample, data))


########################################################################################################################
# Concrete Surface Forms
########################################################################################################################
class Concrete:
    source:         Sample
    realization:    list[str]

    @staticmethod
    def from_sample(sample: Sample, n: int) -> Concrete:
        choices: list[Choices] = [cat.to_lexicon() for cat in sample.sentence]
        # todo: sampling and filtering logic here
        ...


def main(path: str = './prolog/sample.txt') -> list[Sample]: return load_samples(path)


if __name__ == '__main__':
    from argparse import ArgumentParser
    parser = ArgumentParser()
    parser.add_argument('path', type=str, default='./prolog/sample.txt')
    args = parser.parse_args()
    main(args.path)
