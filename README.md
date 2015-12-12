# Fcm
Лабораторная # 1 по ФП.

(c) Снитовец Илья, гр. 151003.

# Параметры
```bash
usage : fhaskell.exe inFile [-o  OUTFILE] [-m  METRIC] [-c  COUNT] [-a  ACCURACY] [-i  INIT] [-h] [--version]

mandatory arguments:
 inFile                        CSV file you want to parse

optional arguments:
 -o, --outFile  OUTFILE        Output file for result of parsing
 -m, --metric  METRIC          Metric for distant count
                               ('euclid', 'hamming')
 -c, --count  COUNT            Count of centers of clasters
 -a, --accuracy  ACCURACY      Accuracy of calculations
 -i, --init  INIT              Type of initialization ('matrix',
                               'centers')
 -h, --help                    show this help message and exit
 --version                     print the program version and exit
```
# Запуск с помощью sandbox
```bash
git clone https://github.com/SnitavetsIV/lab1.git
cd lab1
cabal sandbox init
cabal install --only-dependencies
cabal build
dist\build\fhaskell\fhaskell.exe samples\butterfly.txt -m hamming
```
# Запуск тестов
```bash
cd src
runhaskell Tests
```
