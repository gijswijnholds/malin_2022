from __future__ import annotations
from .preprocessing import SpanDataset, ProcessedSample
from ..populate.generate import *


def contextualize_results(test_data: SpanDataset, predictions: list[list[int]]) -> list[tuple[bool, Context]]:
    return [(pred == true, Context.from_sample(sample, index))
            for sample, preds in zip(test_data, predictions)
            for index, (pred, true) in enumerate(zip(preds, sample.compact.labels))]


def aggr_torch_seeds(results: tuple[list[tuple[bool, Context]], ...]) -> list[tuple[tuple[bool, ...], Context]]:
    return [(tuple(r[0] for r in rs), rs[0][1]) for rs in zip(*results)]


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


def analyze(aggregated: list[tuple[tuple[bool, ...], Context]]) -> None:
    def p(xs: list[tuple[tuple[bool, ...], Context]]) -> str:
        bs, ac, tot = stats(xs)
        return f'{ac} {bs} {tot}'

    aggregated = filter_simple(aggregated)
    total = p(aggregated)
    print(f'{total}: {p(total)}')
    by_num_flippers = group_by_num_flippers(aggregated)
    print('=' * 64)
    for k, vs in by_num_flippers.items(): print(f'{k}: {p(vs)}')
    by_verbal_type = group_by_verbal_type(aggregated)
    print('=' * 64)
    for k, vs in by_verbal_type.items(): print(f'{k}: {p(vs)}')
    by_num_nouns = group_by_num_nouns(aggregated)
    print('=' * 64)
    for k, vs in by_num_nouns.items(): print(f'{k}: {p(vs)}')
    by_num_verbs = group_by_num_verbs(aggregated)
    print('=' * 64)
    for k, vs in by_num_verbs.items(): print(f'{k}: {p(vs)}')
    def has_multiple(results: dict[str, list[tuple[tuple[bool, ...], Context]]]):
        return len([l for l in map(len, results.values()) if l]) > 1
    by_word = group_by_word(aggregated)
    for k, vs in by_word.items():
        by_word_and_cat = group_by_verbal_type(vs)
        print(f'{k}: {p(vs)}')
        if has_multiple(by_word_and_cat):
            for k2, vs2 in by_word_and_cat.items():
                if len(vs2) > 1:
                    print(f'\t\t{k2}: {p(vs2)}')
    # by_word_and_cat = {k: group_by_verbal_type(vs) for k, vs in by_word.items()}
    # by_word_and_cat_count = {k: [len(vs2) for k2, vs2 in vs.items()] for k, vs in by_word_and_cat.items()}
    # words_with_multiple = [k for k in by_word_and_cat_count if len([v for v in by_word_and_cat_count[k] if v != 0]) > 1]
    print('=' * 64)
    for k, vs in by_word.items(): print(f'{k}: {p(vs)}')
    by_head = group_by_head_type(aggregated)
    print('=' * 64)
    for k, vs in by_head.items():
        print(f'{k}: {p(vs)}')
        head_by_verbal_type = group_by_verbal_type(vs)
        for k2, vs2 in head_by_verbal_type.items():
            print(f'\t\t{k2}: {p(vs2)}')
    print('=' * 64)
