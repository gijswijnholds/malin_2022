import os
import csv
from typing import Callable

_data_dir = "../data"
_names_path = os.path.join(_data_dir, 'Top_eerste_voornamen_NL_2010.csv')
_de_noun_path = os.path.join(_data_dir, 'de_personen.txt')
_het_noun_path = os.path.join(_data_dir, 'het_personen.txt')
_inanimate_objects_path = os.path.join(_data_dir, 'inanimate_objects.txt')

_trans_inf_animate_obj_path = os.path.join(_data_dir, 'trans_infinitives_animate_object.txt')
_trans_inf_inanimate_obj_path = os.path.join(_data_dir, 'trans_infinitives_inanimate_object.txt')

_obj_cv_path = os.path.join(_data_dir, 'obj_control_verbs.txt')
_sub_cv_path = os.path.join(_data_dir, 'sub_control_verbs.txt')

_intrans_inf_verbs_path = os.path.join(_data_dir, 'intrans_infinitives.txt')
_verb_raisers_extraposition_path = os.path.join(_data_dir, 'verb_raisers_extraposition.txt')
_verb_raisers_intrans_path = os.path.join(_data_dir, 'verb_raisers_intrans.txt')
_verb_raisers_trans_path = os.path.join(_data_dir, 'verb_raisers_trans.txt')


_adv_path = os.path.join(_data_dir, 'adverbs.txt')


def _load_plain(path: str) -> list[str]:
    return [ln.strip() for ln in open(path, 'r').readlines() if '_' not in ln]


def _load_control_verbs(path: str) -> tuple[list[str], list[str]]:
    with open(path, 'r') as infile:
        verbs_present, verbs_inf = zip(*[ln.strip().split('\t') for ln in infile.readlines()])
    return list(verbs_present), list(verbs_inf)


def _load_names(path: str) -> list[str]:
    with open(path, newline='', encoding='ISO-8859-1') as csvfile:
        reader = csv.reader(csvfile, delimiter=';', quotechar='|')
        female_names, male_names = zip(*[(ln[1], ln[3]) for ln in reader])
    female_names = [name for name in female_names[2:] if name and '?' not in name]
    male_names = [name for name in male_names[2:] if name]
    return female_names + male_names


_names = _load_names(_names_path)

_de_nouns = _load_plain(_de_noun_path)
_het_nouns = _load_plain(_het_noun_path)

_inanimate_objects = _load_plain(_inanimate_objects_path)

_obj_control_verbs_present, _obj_control_verbs_inf = _load_control_verbs(_obj_cv_path)
_subj_control_verbs_present, _subj_control_verbs_inf = _load_control_verbs(_sub_cv_path)

_intrans_inf_verbs = _load_plain(_intrans_inf_verbs_path)

_trans_inf_verbs_animate_obj = _load_plain(_trans_inf_animate_obj_path)
_trans_inf_verbs_inanimate_obj = _load_plain(_trans_inf_inanimate_obj_path)

_verb_raisers_intrans = _load_plain(_verb_raisers_intrans_path)
_verb_raisers_trans = _load_plain(_verb_raisers_trans_path)
_verb_raisers_extraposition = _load_plain(_verb_raisers_extraposition_path)

# _adverbs = _load_plain(_adv_path)



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
        return list(iter(_intrans_inf_verbs))

    @staticmethod
    def inf1():
        return list(iter(_trans_inf_verbs_inanimate_obj))

    @staticmethod
    def inf1a():
        return list(iter(_trans_inf_verbs_animate_obj))

    @staticmethod
    def ivr0():
        return list(iter(_verb_raisers_intrans))

    @staticmethod
    def ivr1():
        return list(iter(_verb_raisers_trans))

    @staticmethod
    # IVR2 = INF2, for verbs that occur both in verb clusters and in extraposition
    def ivr2():
        return list(iter(_verb_raisers_extraposition))

    @staticmethod
    def inf2():
        return list(iter(_verb_raisers_extraposition))

    @staticmethod
    def inf3():
        return list(iter(_obj_control_verbs_inf))

    @staticmethod
    def inf4():
        return list(iter(_subj_control_verbs_inf))

    @staticmethod
    def obj1a():
        # We could also use _de_personen here
        return list(iter(_names))

    @staticmethod
    def obj1i():
        return list(iter(_inanimate_objects))

    @staticmethod
    def obj2():
        # We could also use _de_personen here
        return list(iter(_names))

    @staticmethod
    def pref1():
        return ["hij"]

    @staticmethod
    def pref2():
        return ["zal"]

    @staticmethod
    def te():
        return ["te"]