import os

import torch

from .sparse import SparseVA
from .preprocessing import prepare_dataset
from .trainer import Trainer, make_tester, Maybe
from .analysis import analysis
# from .analysis import agg_torch_seeds
from .model_names import bertje_name, robbert_name

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


def test_probe(data_file: str, bert_name: str, weight_path: str, device: str = 'cuda') -> float:
    _, seed, epoch = weight_path.split('/')[-1].split('_')
    print(f'Testing with seed {seed} @ epoch {epoch}')
    trainer = setup_trainer(data_file, bert_name, True, device=device, model_path=weight_path)
    return analysis(trainer.test_loader.dataset, trainer.predict_epoch())


def do_everything(data_dir: str, bert_names: list[str], weight_dir: str, device: str = 'cuda') -> None:
    results = dict()
    for bert_name in bert_names:
        results[bert_name] = dict()
        print(f'Testing with {bert_name}')
        for data_file in filter(lambda fn: fn.endswith('json'), os.listdir(data_dir)):
            results[bert_name][data_file] = dict()
            for weight_path in filter(lambda fn: fn.startswith(bert_name.split('/')[-1]), os.listdir(weight_dir)):
                accuracy = test_probe(f'{data_dir}/{data_file}', bert_name, f'{weight_dir}/{weight_path}', device)
                print(accuracy)
    print('done')


# do_everything(data_dir='.', bert_names = [bertje_name, robbert_name], weight_dir='./weights')