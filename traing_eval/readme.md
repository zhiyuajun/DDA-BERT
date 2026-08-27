## Environment Setup

1. Create a new Conda environment:

```bash
conda create --name DDAbert python=3.10
```

2. Activate the environment:

```bash
conda activate DDAbert
```

3. Install dependencies:

```bash
pip install -r ./requirements.txt
```



## Download Required Files

Before training or evaluating DDA-BERT, download the pretrained model and example training data from Google Drive.

### 1. Download the pretrained model

Download the pretrained DDA-BERT model from Google Drive:

**https://drive.google.com/drive/folders/1RIF3EEb0YFouXzaIpznpxoj6SpaIpMOI?usp=drive_link**

Place the downloaded model file:

```text
mp_rank_00_model_states.pt
```

in:

```text
/DDA_BERT/software/resource/model/
```

The expected path is:

```text
/DDA_BERT/software/resource/model/mp_rank_00_model_states.pt
```

### 2. Download the example training data

Download the example training files from Google Drive:

**https://drive.google.com/drive/folders/1C1tQX9E8p0slFzZcjmU8SAR-9DPGHSPV?usp=sharing**

Place the downloaded training files in:

```text
/DDA_BERT/traing_eval/train/
```

The directory structure should look like:

```text
DDA_BERT/
├── software/
│   └── resource/
│       └── model/
│           └── mp_rank_00_model_states.pt
│
└── traing_eval/
    ├── train/
    │   └── train_ipc1980_2_dataset114_1.pkl
    ├── train.py
    ├── ds_config.json
    └── yaml/
        └── model.yaml
```

Make sure both the pretrained model and training data are placed in the correct directories before running the following commands.


## Training and Evaluation

### Step 1. Train DDA-BERT

Run DDA-BERT training using DeepSpeed:

```bash
deepspeed --bind_cores_to_rank /DDA_BERT/traing_eval/train.py \
  --deepspeed \
  --deepspeed_config /DDA_BERT/traing_eval/ds_config.json \
  --node_num 1 \
  --gpu_num 8 \
  --config /DDA_BERT/traing_eval/yaml/model.yaml
```

The example training data used by this configuration should be downloaded in advance and placed under:

```text
/DDA_BERT/traing_eval/train/
```

### Step 2. Evaluate DDA-BERT and select the best model

After training, evaluate the trained models to identify the best-performing checkpoint:

```bash
cd /DDA_BERT/traing_eval
python eval.py --config /DDA_BERT/traing_eval/yaml/eval_model.yaml
```
