from __future__ import annotations
from .preprocessing import SpanDataset, ProcessedSample
from ..populate.generate import *


def analysis(test_data: SpanDataset, predictions: list[list[int]]) -> list[tuple[bool, Context]]:
    return [(pred == true, Context.from_sample(sample, index))
            for sample, preds in zip(test_data, predictions)
            for index, (pred, true) in enumerate(zip(preds, sample.compact.labels))]


def aggr_torch_seeds(results: tuple[list[tuple[bool, Context]], ...]) -> list[tuple[tuple[bool, ...], Context]]:
    return [(tuple(r[0] for r in rs), rs[0][1]) for rs in zip(*results)]


class Context(NamedTuple):
    category:       Category
    word:           str
    num_flippers:   int
    term:           Term
    ast:            AST
    sentence:       list[Category]

    @staticmethod
    def count_flippers(matchings: list[tuple[Category, Category]]) -> int:
        # todo: make sure this is working
        return len(set([n for _, n in matchings])) - 1

    @staticmethod
    def from_sample(sample: ProcessedSample, index: int) -> Context:
        source = sample.compact.source
        # TODO: take care to exclude results for sentence with only 1 noun
        # TODO: take care to exclude the (PREF2, PREF1) matching
        ast, term, sentence, matching = (eval(source['ast']), eval(source['term']),
                                         eval(source['sentence']), eval(source['matching']))
        verb_category, noun_category = matching[index]
        num_flippers = Context.count_flippers(matching[:sentence.index(verb_category) + 1])
        word = sample.compact.sentence[sentence.index(verb_category)]
        return Context(category=verb_category,
                       word=word,
                       num_flippers=num_flippers,
                       term=term,
                       ast=ast,
                       sentence=sentence)


def gather(fn, preds: list[tuple[tuple[bool, ...], Context]]) -> list[tuple[tuple[bool, ...], Context]]:
    return [(ps, c) for (ps, c) in preds if fn(c)]


def group_by_verb_category(preds: list[tuple[tuple[bool, ...], Context]]) -> dict[Category, tuple[bool, ...]]:
    return {category: sum((vs for vs, _ in gather(lambda c: c.category == category, preds)), ())
            for category in Category}


def group_by_verbal_type(gathered_by_cat: dict[Category, tuple[bool, ...]]) -> tuple[tuple[bool, ...],
                                                                                     tuple[bool, ...],
                                                                                     tuple[bool, ...]]:
    def is_raiser(c: Category) -> bool: return c in [Category.IVR0, Category.IVR1, Category.IVR2]
    def is_xpos(c: Category) -> bool: return c in [Category.INF2, Category.INF3, Category.INF4]
    def is_inf(c: Category) -> bool: return c in [Category.INF0, Category.INF1, Category.INF1A]
    return (sum((vs for k, vs in gathered_by_cat.items() if is_raiser(k)), ()),
            sum((vs for k, vs in gathered_by_cat.items() if is_xpos(k)), ()),
            sum((vs for k, vs in gathered_by_cat.items() if is_inf(k)), ()))


def group_by_semterm(
        preds: list[tuple[tuple[bool, ...], Context]]) -> dict[Term, list[tuple[tuple[bool, ...], Context]]]:
    unique_terms = set(c.term for _, c in preds)
    return {term: gather(lambda c: c.term == term, preds) for term in unique_terms}


def group_by_sentence(
        preds: list[tuple[tuple[bool, ...], Context]]) -> dict[Term, list[tuple[tuple[bool, ...], Context]]]:
    unique_sents = set(tuple(c.sentence for _, c in preds))
    return {sent: gather(lambda c: c.sentence == sent, preds) for sent in unique_sents}


def group_by_ast(preds: list[tuple[tuple[bool, ...], Context]]) -> dict[AST, list[tuple[tuple[bool, ...], Context]]]:
    unique_asts = set(c.ast for _, c in preds)
    return {ast: gather(lambda c: c.ast == ast, preds) for ast in unique_asts}


def group_semterms_by_different_derivations(
        preds: list[tuple[tuple[bool, ...], Context]]) -> dict[Term, dict[AST, tuple[bool, ...]]]:
    by_term: dict[Term, list[tuple[tuple[bool, ...], Context]]] = group_by_semterm(preds)
    by_pair: dict[Term, dict[AST, list[tuple[tuple[bool, ...], Context]]]] = {k: group_by_ast(vs) for k, vs in by_term.items()}
    return {term: {ast: sum((sum(left, ()) for left, right in vs), ()) for ast, vs in inner_dict.items()}
            for term, inner_dict in by_pair.items() if len(inner_dict) > 1}


def group_sentences_by_different_derivations(
        preds: list[tuple[tuple[bool, ...], Context]]) -> dict[Term, dict[AST, tuple[bool, ...]]]:
    by_sent = group_by_semterm(preds)
    by_pair = {k: group_by_ast(vs) for k, vs in by_sent.items()}
    return {term: {ast: sum((sum(left, ()) for left, right in vs), ()) for ast, vs in inner_dict.items()}
            for term, inner_dict in by_pair.items() if len(inner_dict) > 1}


def group_by_num_flippers(
        preds: list[tuple[tuple[bool, ...], Context]]) -> dict[int, tuple[tuple[bool, ...]]]:
    flippers = set(c.num_flippers for _, c in preds)
    return {flipper: sum((vs for vs, _ in gather(lambda c: c.num_flippers == flipper, preds)), ())
            for flipper in flippers}
