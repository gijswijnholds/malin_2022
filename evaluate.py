import os

import torch

from src.evaluation.sparse import SparseVA
from src.evaluation.preprocessing import prepare_dataset
from src.evaluation.trainer import Trainer, make_tester, Maybe
from src.evaluation.analysis import contextualize_results, Context, aggr_torch_seeds
from src.evaluation.model_names import bertje_name, robbert_name

GLOBAL_SEEDS = [3, 7, 42]


def setup_trainer(
        data_path: str,
        bert_name: str,
        freeze: bool,
        device: str,
        seed: int = 42,
        model_path: Maybe[str] = None) -> Trainer:
    word_pad_id = 3 if bert_name == bertje_name else 1 if bert_name == robbert_name else None
    torch.manual_seed(seed)
    model = SparseVA(bert_name=bert_name, freeze=freeze, dim=768, selection_h=128)
    test_ds = prepare_dataset(data_path, bert_name)
    assert model_path is not None
    model.load(model_path)
    model.to(device)
    return make_tester(
        name=f'{bert_name.split("/")[-1]}_{seed}',
        model=model,
        test_dataset=test_ds,
        batch_size_test=128,
        device=device,
        word_pad_id=word_pad_id)


def test_probe(data_file: str, bert_name: str, weight_path: str, device: str = 'cuda') -> list[tuple[bool, Context]]:
    _, seed, epoch = weight_path.split('/')[-1].split('_')
    print(f'Testing with seed {seed} @ epoch {epoch}')
    trainer = setup_trainer(data_file, bert_name, True, device=device, model_path=weight_path)
    return contextualize_results(trainer.test_loader.dataset, trainer.predict_epoch())


def do_everything(data_dir: str, bert_names: list[str], weight_dir: str, device: str = 'cuda') -> dict:
    results = dict()
    for bert_name in bert_names:
        results[bert_name] = dict()
        print(f'Testing with {bert_name}')
        data_file = next(filter(lambda fn: fn.endswith('json'), os.listdir(data_dir)))
        results[bert_name] = dict()
        for weight_path in filter(lambda fn: fn.startswith(bert_name.split('/')[-1]), os.listdir(weight_dir)):
            results[bert_name][weight_path] = test_probe(f'{data_dir}/{data_file}',
                                                         bert_name,
                                                         f'{weight_dir}/{weight_path}',
                                                         device)
        results[bert_name] = aggr_torch_seeds(tuple(v for v in results[bert_name].values()))
    return results


if __name__ == '__main__':
    results = do_everything('/', bert_names=[bertje_name], weight_dir='./evaluation/weights')
    import pickle
    with open('results.p', 'wb') as f:
        pickle.dump(results, f)
