from typing import NamedTuple, Any
import json

Matching = dict[int, int]
Realized = list[tuple[list[int], list[int], str]]


Source = Any


class CompactSample(NamedTuple):
    source:     Source
    sentence:   list[str]
    n_spans:    list[list[int]]
    v_spans:    list[list[int]]
    labels:     list[int]


def fix_matching(matching: dict[str, int]) -> Matching:
    return {int(k): matching[k] for k in matching}


def process_grammar(data: list) -> list[tuple[Source, Matching, Realized]]:
    return [(sample['source'], fix_matching(eval(sample['matching'])), eval(sample['realization'])) for sample in data]


def expand_spans(idss: list[list[int]], idx: int):
    return [1 if idx in ids else 0 for ids in idss]


def make_sample(source: Source, matching: Matching, realization: Realized) -> CompactSample:
    nss, vss, ws = zip(*realization)
    n_ids = set(sum(nss, []))
    v_ids = set(sum(vss, []))
    noun_spans = list(map(lambda ni: expand_spans(nss, ni), n_ids))
    verb_spans = list(map(lambda vi: expand_spans(vss, vi), v_ids))
    labels_out = list(map(lambda k: matching[k], matching))
    return CompactSample(source, ws, noun_spans, verb_spans, labels_out)


def read_file(grammar_fn: str) -> list[CompactSample]:
    with open(grammar_fn, 'r') as inf:
        data = json.load(inf)
    return [make_sample(*x) for x in process_grammar(data)]
