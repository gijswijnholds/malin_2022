from __future__ import annotations
from enum import Enum, auto
from typing import NamedTuple
from src.populate.lexicon import Lexicon
import random
import json

Choices = list[str]


########################################################################################################################
# Categories
########################################################################################################################
class Category(Enum):
    INF0 = (auto(), True)
    INF1 = (auto(), True)
    INF1A = (auto(), True)
    IVR0 = (auto(), True)
    IVR1 = (auto(), True)
    IVR2 = (auto(), True)
    INF2 = (auto(), True)
    INF3 = (auto(), True)
    INF4 = (auto(), True)
    OBJ1A = (auto(), False)
    OBJ1I = (auto(), False)
    OBJ2 = (auto(), False)
    PREF1 = (auto(), False)
    PREF2 = (auto(), True)
    TE = (auto(), None)

    def to_lexicon(self) -> Choices: return eval(f'Lexicon.{self.name.lower()}()')
    def is_noun(self) -> bool: return self.value[1] is not None and not self.value[1]
    def is_verb(self) -> bool: return self.value[1] is not None and self.value[1]
    def __repr__(self) -> str: return f'Category.{self.name}'
    def __eq__(self, other) -> bool: return isinstance(other, Category) and self.name == other.name
    def __hash__(self) -> int: return hash(self.name)


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
    def __eq__(self, other) -> bool: raise NotImplementedError
    def __hash__(self) -> int: raise NotImplementedError


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
    def __eq__(self, other) ->  bool: return isinstance(other, Rule) and self.name == other.name
    def __hash__(self) -> int: return hash(self.name)


class RuleTree(AST):
    def __init__(self, left: AST, right: AST): self.left = left; self.right = right
    def __repr__(self) -> str: return f'({repr(self.left)}, {repr(self.right)})'
    def __len__(self) -> int: return 1 + max(len(self.left), len(self.right))

    def __eq__(self, other) -> bool:
        return isinstance(other, RuleTree) and self.left == other.left and self.right == other.right

    def __hash__(self) -> int:
        return hash((self.left, self.right))


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
    def __hash__(self) -> int: raise NotImplementedError
    def __eq__(self, other) -> bool: raise NotImplementedError


class Application(Term):
    def __init__(self, function: Term | Category, argument: Term | Category):
        self.function = function
        self.argument = argument

    def __len__(self) -> int: return 1 + max(len(self.function), len(self.argument))
    def __repr__(self) -> str: return f'appl({repr(self.function)}, {repr(self.argument)})'

    def __eq__(self, other) -> bool:
        return isinstance(other, Application) and self.function == other.function and self.argument == other.argument

    def __hash__(self) -> int: return hash((self.function, self.argument))


class DiaElim(Term):
    def __init__(self, diamond: str, body: Term | Category):
        self.diamond = diamond
        self.body = body

    def __len__(self) -> int: return len(self.body)
    def __repr__(self) -> str: return f'condia({self.diamond}, {repr(self.body)})'

    def __eq__(self, other) -> bool:
        return isinstance(other, DiaElim) and self.diamond == other.diamond and self.body == other.body

    def __hash__(self) -> int: return hash((self.diamond, self.body))


vc = 'vc'
su = 'su'
obj1 = 'obj1'
obj2 = 'obj2'


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


def eval_ast(line: str) -> AST: return eval(line)
def eval_matchings(line: str) -> list[tuple[Category, Category]]: return eval(line)
def eval_sentence(line: str) -> list[Category]: return list(map(eval, line.split()))
def eval_semterm(line: str) -> Term: return eval(line)


def make_sample(lines: list[str]) -> Sample:
    return Sample(*[f(x) for f, x in zip([eval_ast, eval_sentence, eval_semterm, eval_matchings], lines)])


def load_samples(path: str) -> list[Sample]:
    with open(path, 'r') as in_file:
        data = list(map(lambda ln: ln.split('\n'), in_file.read().strip().split('\n\n')))
    return list(map(make_sample, data))


def try_unique_sample(xs: list, n: int) -> list:
    if len(xs) > n:
        return random.sample(xs, n)
    else:
        return random.sample(xs, len(xs)) + [random.choice(xs) for _ in range(n - len(xs))]


########################################################################################################################
# Concrete Surface Forms
########################################################################################################################
NounIndices = list[int]
VerbIndices = list[int]
Word = str
Realization = list[tuple[NounIndices, VerbIndices, Word]]
Matching = dict[int, int]


class Concrete:
    def __init__(self, source: Sample, realization: Realization, matching: Matching):
        assert len(realization) == len(source.sentence)
        self.source = source
        self.realization: Realization = realization
        self.matching: Matching = matching

    @staticmethod
    def to_indices(xs: list[bool]) -> list[list[int]]:
        num_members = iter(range(xs.count(True)))
        indices = [[next(num_members)] if x else [] for x in xs]
        return indices

    @staticmethod
    def from_sample(sample: Sample, n: int) -> list[Concrete]:
        choices: list[Choices] = [cat.to_lexicon() for cat in sample.sentence]
        wordss = list(zip(*[try_unique_sample(choice, n) for choice in choices]))
        verb_indices = Concrete.to_indices(list(map(Category.is_verb, sample.sentence)))
        noun_indices = Concrete.to_indices(list(map(Category.is_noun, sample.sentence)))
        matching = {vs[0]: noun_indices[sample.sentence.index(noun_cat)][0]
                    for verb_cat, noun_cat in sample.matchings
                    if (vs := verb_indices[sample.sentence.index(verb_cat)])}
        return [Concrete(sample, list(zip(noun_indices, verb_indices, words)), matching) for words in wordss]

    def as_dict(self) -> dict:
        return {'source': {'ast': repr(self.source.ast),
                           'sentence': repr(self.source.sentence),
                           'term': repr(self.source.term),
                           'matching': repr(self.source.matchings)},
                'realization': repr(self.realization),
                'matching': repr(self.matching)}
    
    @staticmethod
    def from_dict(d) -> Concrete:
        source = Sample(ast=eval(d['source']['ast']), sentence=eval(d['source']['sentence']),
                        term=eval(d['source']['term']), matchings=eval(d['source']['matching']))
        return Concrete(source=source, realization=eval(d['realization']), matching=eval(d['matching']))


def load_concrete_samples(path: str = './prolog/sample.txt', n: int = 10) -> list[Concrete]:
    return [c for s in load_samples(path) for c in Concrete.from_sample(s, n)]


def dump_to_json(c_samples: list[Concrete], path: str):
    with open(path, 'w') as f:
        json.dump([c.as_dict() for c in c_samples], f, indent=4)
