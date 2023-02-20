#!/bin/env python3
import argparse

parser = argparse.ArgumentParser(
    description="Returns percentage of matching lines from two files.")
parser.add_argument('file1', type=str) # test file
parser.add_argument('file2', type=str) # predicted file
parser.add_argument('--n', default=0, type=int) # top n
parser.add_argument('--save_covers', action='store_true')
args = parser.parse_args()

with open(args.file1, 'r') as f:
    labels = f.read().splitlines()

with open(args.file2, 'r') as f:
    ranks = f.read().splitlines()

assert len(labels) == len(ranks)

for i in range(len(ranks)):
    labels[i] = labels[i].split(' ')
    ranks[i] = ranks[i].split(' ')[ : len(labels[i]) + args.n ]

covers = [len(set(x[0]) & set(x[1])) / len(x[0]) for x in zip(labels, ranks)]
cover = sum(covers) / len(covers)
print(f"Cover, plus {str(args.n)}: {cover * 100:.2f}%")

if args.save_covers:
    path = f'{args.file2}.covers-{args.n}'
    lines = '\n'.join([str(i) for i in covers]) + '\n'
    with open(path, 'w') as f:
        f.write(lines)

