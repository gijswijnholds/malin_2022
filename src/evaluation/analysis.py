from __future__ import annotations

import pdb
from typing import Any
from .preprocessing import SpanDataset, ProcessedSample
from ..populate.generate import *


def contextualize_results(test_data: SpanDataset, predictions: list[list[int]]) -> list[tuple[bool, Context]]:
    return [(pred == true, Context.from_sample(sample, index))
            for sample, preds in zip(test_data, predictions)
            for index, (pred, true) in enumerate(zip(preds, sample.compact.labels))]


def aggr_torch_seeds(results: tuple[list[tuple[bool, Context]], ...]) -> list[tuple[tuple[bool, ...], Context]]:
    return [(tuple(r[0] for r in rs), rs[0][1]) for rs in zip(*results)]


def _collate_raise_and_extra(cat: Category) -> Category:
    return Category.META_INF if cat in {Category.INF2, Category.IVR2} else cat


def collate_raise_and_extra(term: Term) -> Term: return term_fmap(_collate_raise_and_extra, term)


def _collate_control(cat: Category) -> Category:
    return Category.META_CTRL if cat in {Category.INF3, Category.INF4} else cat


def collate_control(term: Term) -> Term: return term_fmap(_collate_control, term)


class Context(NamedTuple):
    category:       Category
    dominated_by:   Category | None
    word:           str
    num_flippers:   int
    term:           Term
    ast:            AST
    sentence:       list[Category]

    def num_nouns(self) -> int: return sum([c.is_noun() for c in self.sentence])
    def num_verbs(self) -> int: return sum([c.is_verb() for c in self.sentence])
    def easy(self) -> bool: return self.category == Category.PREF2 or self.num_nouns() == 1

    @staticmethod
    def count_flippers(matchings: list[tuple[Category, Category]]) -> int:
        return len(set([n for _, n in matchings])) - 1

    @staticmethod
    def from_sample(sample: ProcessedSample, index: int) -> Context:
        source = sample.compact.source
        # TODO: take care to exclude results for sentence with only 1 noun
        # TODO: take care to exclude the (PREF2, PREF1) matching
        ast, term, sentence, matching = (eval(source['ast']), eval(source['term']),
                                         eval(source['sentence']), eval(source['matching']))
        verb_category, noun_category = matching[index]
        num_flippers = Context.count_flippers(matching[:index + 1])
        word = sample.compact.sentence[sentence.index(verb_category)]
        return Context(category=verb_category,
                       dominated_by=next((v for v in sentence[sentence.index(verb_category)-1::-1] if v.is_verb()),
                                         None),
                       word=word,
                       num_flippers=num_flippers,
                       term=term,
                       ast=ast,
                       sentence=sentence)


def is_raiser(c: Category) -> bool: return c in [Category.IVR0, Category.IVR1, Category.IVR2]
def is_xpos(c: Category) -> bool: return c in [Category.INF2, Category.INF3, Category.INF4]
def is_pref2(c: Category) -> bool: return c == Category.PREF2
def is_inf(c: Category) -> bool: return c in [Category.INF0, Category.INF1, Category.INF1A]


def gather(fn, preds: list[tuple[tuple[bool, ...], Context]]) -> list[tuple[tuple[bool, ...], Context]]:
    return [(ps, c) for (ps, c) in preds if fn(c)]


def group_by_position(preds: list[tuple[tuple[bool, ...], Context]], cat: Category) -> tuple[list[tuple[tuple[bool, ...], Context]],
                                                                                             list[tuple[tuple[bool, ...], Context]],
                                                                                             list[tuple[tuple[bool, ...], Context]]]:
    def left_of_cat(c: Context) -> bool: return c.sentence.index(c.category) < c.sentence.index(cat)
    def right_of_cat(c: Context) -> bool: return c.sentence.index(c.category) > c.sentence.index(cat)
    def is_cat(c: Context) -> bool: return c.category == cat
    return (gather(left_of_cat, preds),
            gather(right_of_cat, preds),
            gather(is_cat, preds))


def group_by_word(preds: list[tuple[tuple[bool, ...], Context]]) -> dict[str, list[tuple[tuple[bool, ...], Context]]]:
    return {word: gather(lambda c: c.word == word, preds) for word in set(c.word for _, c in preds)}


def group_by_verb_category(preds: list[tuple[tuple[bool, ...], Context]]) -> dict[Category, tuple[bool, ...]]:
    return {category: sum((vs for vs, _ in gather(lambda c: c.category == category, preds)), ())
            for category in Category}


def group_by_verbal_type(preds: list[tuple[tuple[bool, ...], Context]]) \
        -> dict[str, list[tuple[tuple[bool, ...], Context]]]:
    return {'raiser': gather(lambda c: is_raiser(c.category), preds),
            'xpos': gather(lambda c: is_xpos(c.category), preds),
            'inf': gather(lambda c: is_inf(c.category), preds)}


def group_by_head_type(preds: list[tuple[tuple[bool, ...], Context]]) \
        -> dict[str, list[tuple[tuple[bool, ...], Context]]]:
    return {'raiser': gather(lambda c: is_raiser(c.dominated_by), preds),
            'xpos': gather(lambda c: is_xpos(c.dominated_by), preds),
            'undom': gather(lambda c: is_pref2(c.dominated_by), preds)}


def group_by_semterm(
        preds: list[tuple[tuple[bool, ...], Context]],
        fmap: Callable[[Term], Term]) -> dict[Term, list[tuple[tuple[bool, ...], Context]]]:
    unique_terms = set(fmap(c.term) for _, c in preds)
    return {term: gather(lambda c: fmap(c.term) == term, preds) for term in unique_terms}


def group_by_sentence(
        preds: list[tuple[tuple[bool, ...], Context]]) -> dict[tuple[Category, ...], list[tuple[tuple[bool, ...], Context]]]:
    unique_sents = set(tuple(c.sentence) for _, c in preds)
    return {sent: gather(lambda c: tuple(c.sentence) == sent, preds) for sent in unique_sents}


def group_by_ast(preds: list[tuple[tuple[bool, ...], Context]]) -> dict[AST, list[tuple[tuple[bool, ...], Context]]]:
    unique_asts = set(c.ast for _, c in preds)
    return {ast: gather(lambda c: c.ast == ast, preds) for ast in unique_asts}


def group_semterms_by_different_derivations(
        preds: list[tuple[tuple[bool, ...], Context]]) -> dict[bool, dict[str, list[tuple[tuple[bool, ...], Context]]]]:
    by_term: dict[Term, list[tuple[tuple[bool, ...], Context]]] = group_by_semterm(preds, collate_raise_and_extra)
    by_term_squared = {k: group_by_semterm(vs, lambda x: x) for k, vs in by_term.items()}
    by_term_squared = {k: vs for k, vs in by_term_squared.items() if has_multiple(vs)}
    ret = {True: {'left': [], 'verb': [], 'right': []}, False: {'left': [], 'verb': [], 'right': []}}
    for key in by_term_squared:
        here, there = tuple(by_term_squared[key].keys())
        here_cat, there_cat = find_diff(here, there)
        here_raiser = is_raiser(here_cat)
        left, right, verb = group_by_position(by_term_squared[key][here], here_cat)
        ret[here_raiser]['left'] += left
        ret[here_raiser]['right'] += right
        ret[here_raiser]['verb'] += verb
        left, right, verb = group_by_position(by_term_squared[key][there], there_cat)
        ret[not here_raiser]['left'] += left
        ret[not here_raiser]['right'] += right
        ret[not here_raiser]['verb'] += verb
    return ret


def group_semterms_by_control(
        preds: list[tuple[tuple[bool, ...], Context]]) -> dict[str, list[tuple[tuple[bool, ...], Context]]]:
    by_term: dict[Term, list[tuple[tuple[bool, ...], Context]]] = group_by_semterm(preds, collate_control)
    by_term = {k: [(ps, c) for ps, c in vs if c.dominated_by in {Category.INF3, Category.INF4}]
               for k, vs in by_term.items()}
    by_term_squared = {k: group_by_semterm(vs, lambda x: x) for k, vs in by_term.items()}
    by_term_squared = {k: vs for k, vs in by_term_squared.items() if has_multiple(vs)}
    ret: dict[str, list[tuple[tuple[bool, ...], Context]]] = {'inf3': [], 'inf4': []}
    for k, vs in by_term_squared.items():
        assert len(vs) == 2
        key1, key2 = tuple(vs.keys())
        assert len(vs[key1]) == len(vs[key2]) == 10
        cat1, cat2 = find_diff(key1, key2)
        inf3, inf4 = (vs[key1], vs[key2]) if cat1 == Category.INF3 else (vs[key2], vs[key1])
        ret['inf3'] += inf3
        ret['inf4'] += inf4
    return ret


def has_multiple(results: dict[Any, list[tuple[tuple[bool, ...], Context]]]):
    return len([l for l in map(len, results.values()) if l]) > 1


def find_diff(here: Term, there: Term) -> tuple[Category, Category]:
    if isinstance(here, Application):
        assert isinstance(there, Application)
        if here.function == there.function:
            return find_diff(here.argument, there.argument)
        else:
            return find_diff(here.function, there.function)
    elif isinstance(here, DiaElim):
        assert isinstance(there, DiaElim)
        return find_diff(here.body, there.body)
    else:
        assert isinstance(here, Category) and isinstance(there, Category)
        return here, there


def group_by_num_flippers(
        preds: list[tuple[tuple[bool, ...], Context]]) -> dict[int, list[tuple[tuple[bool, ...], Context]]]:
    flippers = set(c.num_flippers for _, c in preds)
    return {flipper: gather(lambda c: c.num_flippers == flipper, preds) for flipper in flippers}


def group_by_num_nouns(
        preds: list[tuple[tuple[bool, ...], Context]]) -> dict[int, list[tuple[tuple[bool, ...], Context]]]:
    return {n: gather(lambda c: c.num_nouns() == n, preds) for n in set(c.num_nouns() for _, c in preds)}


def group_by_num_verbs(
        preds: list[tuple[tuple[bool, ...], Context]]) -> dict[int, list[tuple[tuple[bool, ...], Context]]]:
    return {n: gather(lambda c: c.num_verbs() == n, preds) for n in set(c.num_verbs() for _, c in preds)}


def filter_simple(preds: list[tuple[tuple[bool, ...], Context]]) -> list[tuple[tuple[bool, ...], Context]]:
    return [(ps, c) for ps, c in preds if not Context.easy(c)]


def stats(xs: list[tuple[tuple[bool, ...], Context]]) -> tuple[float, float, int]:
    bs, ac = list(zip(*((1/Context.num_nouns(c), p) for ps, c in xs for p in ps)))
    return sum(bs)/len(bs), sum(ac)/len(ac), len(bs)


def group_by_cat(
        preds: list[tuple[tuple[bool, ...], Context]]) -> dict[Category, list[tuple[tuple[bool, ...], Context]]]:
    return {cat: gather(lambda c: c.category == cat, preds) for cat in set(c.category for _, c in preds)}


def group_by_dom_cat(
        preds: list[tuple[tuple[bool, ...], Context]]) -> dict[Category, list[tuple[tuple[bool, ...], Context]]]:
    return {cat: gather(lambda c: c.dominated_by == cat, preds) for cat in set(c.dominated_by for _, c in preds)}


def analyze(aggregated: list[tuple[tuple[bool, ...], Context]]) -> None:
    def p(xs: list[tuple[tuple[bool, ...], Context]]) -> str:
        bs, ac, tot = stats(xs)
        return f'{ac} {bs} {tot}'

    aggregated = filter_simple(aggregated)
    # by_control = group_semterms_by_control(aggregated)
    # pdb.set_trace()
    total = p(aggregated)
    print(f'total: {total}')
    by_num_flippers = group_by_num_flippers(aggregated)
    print('=' * 64)
    for k, vs in by_num_flippers.items(): print(f'{k}: {p(vs)}')
    by_verbal_type = group_by_verbal_type(aggregated)
    print('=' * 64)
    for k, vs in by_verbal_type.items(): print(f'{k}: {p(vs)}')
    by_num_nouns = group_by_num_nouns(aggregated)
    print('=' * 64)
    by_cat = group_by_cat(aggregated)
    for k, vs in by_cat.items(): print(f'{k}: {p(vs)}')
    print('=' * 64)
    by_dom_cat = group_by_dom_cat(aggregated)
    for k, vs in by_dom_cat.items(): print(f'{k}: {p(vs)}')
    print('=' * 64)
    for k, vs in by_num_nouns.items(): print(f'{k}: {p(vs)}')
    by_num_verbs = group_by_num_verbs(aggregated)
    print('=' * 64)
    by_dif_derivations = group_semterms_by_different_derivations(aggregated)
    for k, vs in by_dif_derivations.items():
        print(f'{"raiser" if k else "xpos"}: {p(sum(tuple(vs.values()), []))}')
        for k2, vs2 in vs.items():
            print(f'\t{k2}: {p(vs2)}')
    print('=' * 64)
    for k, vs in by_num_verbs.items(): print(f'{k}: {p(vs)}')
    print('=' * 64)
    by_word = group_by_word(aggregated)
    for k, vs in by_word.items():
        by_word_and_cat = group_by_verbal_type(vs)
        print(f'{k}: {p(vs)}')
        if has_multiple(by_word_and_cat):
            for k2, vs2 in by_word_and_cat.items():
                if len(vs2) > 1:
                    print(f'\t{k2}: {p(vs2)}')
    by_head = group_by_head_type(aggregated)
    print('=' * 64)
    for k, vs in by_head.items():
        print(f'{k}: {p(vs)}')
        head_by_verbal_type = group_by_verbal_type(vs)
        for k2, vs2 in head_by_verbal_type.items():
            print(f'\t{k2}: {p(vs2)}')
    print('=' * 64)
