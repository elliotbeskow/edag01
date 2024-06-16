# include <stdio.h>
# include <stdlib.h>
# include <stdbool.h>
# include <string.h>

typedef struct word_t {
    char *str;
    int freq;
} word_t;

bool is_prime(int number) {
    if (number <= 1) {
        return false;
    }
    if (number == 2) {
        return true;
    }
    if (number % 2 == 0) {
        return false;
    }
    for (int i = 3; i * i <= number; i += 2) {
        if (number % i == 0) {
            return false;
        }
    }
    return true;
}

int main() {
    int n, len, i, size = 0, line = 0;
    char *str = malloc(sizeof(char) * 100);
    word_t *words = calloc(1, sizeof(word_t));
    while (true) {
        line++;
        bool prime = is_prime(line);
        n = scanf("%s\n", str);
        if (n == EOF) {
            break;
        }
        len = strlen(str);
        char *word = calloc(len + 1, sizeof(char));
        strcpy(word, str);
        bool found = false;
        for (i = 0; i < size; i++) {
            if (strcmp(words[i].str, word) == 0) {
                found = true;
                if (prime) {
                    if (words[i].freq == 0) {
                        printf("trying to delete %s: not found\n", word);
                    } else {
                        words[i].freq = 0;
                        printf("trying to delete %s: deleted\n", word);
                    }
                } else {
                    if (words[i].freq == 0) {
                        words[i].freq = 1;
                        printf("added %s\n", word);
                    } else {
                        words[i].freq++;
                        printf("counted %s\n", word);
                    }
                }
                break;
            }
        }
        if (prime && !found) {
            printf("trying to delete %s: not found\n", word);
        }
        if (found || prime) {
            free(word);
            continue;
        }
        size++;
        words = realloc(words, sizeof(word_t) * size);
        words[size - 1].str = word;
        words[size - 1].freq = 1;
        printf("added %s\n", word);
    };
    int max_count = 0;
    char *max_word = malloc(100*sizeof(char));
    for (i = 0; i < size; i++) {
        if (words[i].freq > max_count) {
            max_count = words[i].freq;
            strcpy(max_word, words[i].str);
        } else if (words[i].freq == max_count && strcmp(words[i].str, max_word) < 0) {
            strcpy(max_word, words[i].str);
        }
    }
    printf("result: %s %d\n", max_word, max_count);
    for (i = 0; i < size; i++) {
        free(words[i].str);
    }
    free(words);
    free(str);
    free(max_word);
    return 0;
}