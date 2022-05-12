from __future__ import annotations
from enum import Enum
from typing import NamedTuple


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


# enumeration of canonical constants
class Category(Enum):
    A = ...
    B = ...
    C = ...

    iets = ...
    zal = ...
    haar = ...
    verzoeken = ...
    hun = ...
    laten = ...
    willen = ...
    ontmoeten = ...
    vertrekken = ...
    proberen = ...
    te = ...
    beloven = ...
    zeggen = ...
    hij = ...


iets = Category.A
zal = Category.zal
haar = Category.A
verzoeken = Category.verzoeken
hun = Category.hun
laten = Category.laten
willen = Category.willen
ontmoeten = Category.ontmoeten
vertrekken = Category.vertrekken
proberen = Category.proberen
te = Category.te
beloven = Category.beloven
zeggen = Category.zeggen
hij = Category.hij



# reading
AST = ...
Term = ...
Sample = tuple[AST, list[Category], Term, list[tuple[Category, Category]]]


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

    ast, sentence, sem_term, matchings = lines
    return Sample(eval_ast(ast), eval_sentence(sentence), eval_semterm(sem_term), eval_matchings(matchings))


def load_samples(path: str) -> list[Sample]:
    with open(path, 'r') as in_file:
        data = list(map(lambda ln: ln.split('\n'), in_file.read().strip().split('\n\n')))
    return list(map(make_sample, data))

path = './prolog/sample.txt'
path = './prolog/sample_log.txt'
# samples = load_samples(path)
# def generate(sequence: list[Category]) -> list[list[StringSample]]:
#     pass

if __name__ == '__main__':
    raise NotImplementedError
