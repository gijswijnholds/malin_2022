import os
import csv

_data_dir = "../data"
_names_path = os.path.join(_data_dir, 'Top_eerste_voornamen_NL_2010.csv')
_de_noun_path = os.path.join(_data_dir, 'de_personen.txt')
_het_noun_path = os.path.join(_data_dir, 'het_personen.txt')

_trans_inf_animate_obj_path = os.path.join(_data_dir, 'person_transitive_verbs.txt')

_obj_cv_path = os.path.join(_data_dir, 'obj_control_verbs.txt')
_sub_cv_path = os.path.join(_data_dir, 'sub_control_verbs.txt')

_intrans_inf_verbs_path = os.path.join(_data_dir, 'infinitives.txt')
_verb_raisers_extraposition_path = os.path.join(_data_dir, 'verb_raisers_extraposition.txt')
_verb_raisers_intrans_path = os.path.join(_data_dir, 'verb_raisers_intrans.txt')
_verb_raisers_trans_path = os.path.join(_data_dir, 'verb_raisers_trans.txt')


_adv_path = os.path.join(_data_dir, 'adverbs.txt')
_ipp_itv_path = os.path.join(_data_dir, 'ipp_intrans_infinitives.txt')
_ipp_tv_path = os.path.join(_data_dir, 'ipp_trans_infinitives.txt')
_ipp_itv_te_path = os.path.join(_data_dir, 'ipp_intrans_infinitives_te.txt')



def _load_plain(path: str) -> list[str]:
    return [ln.strip() for ln in open(path, 'r').readlines() if '_' not in ln]


def _load_control_verbs(path: str) -> tuple[list[str], list[str]]:
    with open(path, 'r') as infile:
        verbs_present, verbs_inf = zip(*[ln.strip().split('\t') for ln in infile.readlines()])
    return list(verbs_present), list(verbs_inf)


def _load_names(path: str) -> tuple[list[str], list[str]]:
    with open(path, newline='', encoding='ISO-8859-1') as csvfile:
        reader = csv.reader(csvfile, delimiter=';', quotechar='|')
        female_names, male_names = zip(*[(ln[1], ln[3]) for ln in reader])
    female_names = [name for name in female_names[2:] if name and '?' not in name]
    male_names = [name for name in male_names[2:] if name]
    return female_names + male_names


_names = _load_names(_names_path)


_de_nouns = _load_plain(_de_noun_path)
_het_nouns = _load_plain(_het_noun_path)
_obj_control_verbs_present, _obj_control_verbs_inf = _load_control_verbs(_obj_cv_path)
_sub_control_verbs_present, _sub_control_verbs_inf = _load_control_verbs(_sub_cv_path)

_intrans_inf_verbs = _load_plain(_intrans_inf_verbs_path)
_trans_inf_verbs_animate_obj = _load_plain(_trans_inf_animate_obj_path)
_verb_raisers_intrans = _load_plain(_verb_raisers_intrans_path)
_verb_raisers_trans = _load_plain(_verb_raisers_trans_path)

_adverbs = _load_plain(_adv_path)
_ipp_intransitive_infinitive_verbs = _load_plain(_ipp_itv_path)
_ipp_transitive_infinitive_verbs = _load_plain(_ipp_tv_path)
_ipp_intransitive_infinitive_te_verbs = _load_plain(_ipp_itv_te_path)
_verb_raisers_extraposition = _load_plain(_verb_raisers_extraposition_path)


# Lexical categories/representatives
#
# inf0(d0) --> [d0]. % vertrekken, intransitive infinitive -> DONE
# inf1(d1) --> [d1]. % zeggen, transitive infinitive, inanimate object
# inf1a(d11) --> [d11]. % ontmoeten, transitive infinitive, animate object -> DONE
#
# ivr0(d2) --> [d2]. % willen, obligatory VR -> DONE (verb_raisers_intrans)
# ivr1(d3) --> [d3]. % laten, obligatory VR, direct object is understood subject of vc -> DONE (verb_raisers_trans)
# obj1_(d4) --> [d4]. % haar, animate direct object -> DONE
# obj1(d5) --> [d5]. % iets, inanimate direct object
# obj2(d10) --> [d10]. % hun, indirect object -> DONE (only animate)
#
# ivr2(d6) --> [d6]. % proberen, VR (also: d7) -> DONE (verb_raisers_extraposition)
# inf2(d7) --> [d7]. % dreigen, extraposition -> DONE (verb_raisers_extraposition)
# inf3(d9) --> [d9]. % verzoeken+indirect object, extraposition, indirect object=understood su of vc -> DONE (obj control verbs)
# inf4(d12) --> [d12]. % beloven+indirect object, extraposition, subject=understood su of vc -> DONE (subj control verbs)

class Lexicon:
    @staticmethod
    def inf0():
        return lambda _: list(iter(_intrans_inf_verbs))

    @staticmethod
    def inf1():
        return list(iter(_))

    @staticmethod
    def inf1a():
        return lambda _: list(iter(_trans_inf_verbs_animate_obj))

    @staticmethod
    def ivr0():
        return lambda _: list(iter(_verb_raisers_intrans))

    @staticmethod
    def ivr1():
        return lambda _: list(iter(_verb_raisers_trans))

    @staticmethod
    # IVR2 = INF2, for verbs that occur both in verb clusters and in extraposition
    def ivr2():
        return lambda _: list(iter(_verb_raisers_extraposition))

    @staticmethod
    def inf2():
        return lambda _: list(iter(_verb_raisers_extraposition))

    @staticmethod
    def inf3():
        inf3idict = {Lexicon.obj2()(v) for v in list(iter(_obj_control_verbs_inf))}
        def f(verb) -> list[str]: return inf3idict[verb]
        return f

    @staticmethod
    def inf4():
        inf4idict = {Lexicon.obj2()(v) for v in list(iter(_obj_control_verbs_inf))}
        def f(verb) -> list[str]: return inf4idict[verb]
        return f

    @staticmethod
    def obj1a():
        return lambda _: list(iter(_names))

    @staticmethod
    def obj1i() -> Callable[[str], list[str]]:
        # TODO: map transitive infinitives with an inanimate object to lists of said objects
        inf1idict = ...
        def f(verb) -> list[str]: return inf1idict[verb]
        return f

    @staticmethod
    def obj2():
        return lambda _: list(iter(_names))

    @staticmethod
    def pref1():
        return lambda _: ["hij"]

    @staticmethod
    def pref2():
        return lambda _: ["zal"]

    @staticmethod
    def te():
        return lambda _: ["te"]