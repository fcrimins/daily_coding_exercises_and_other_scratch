#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * https://www.hackerrank.com/challenges/sorting-array-of-strings/problem
 */

int lexicographic_sort(const char* a, const char* b) {
    int len_a = strlen(a);
    int len_b = strlen(b);
    for(int i = 0; i < (len_a < len_b ? len_a : len_b); ++i) {
        if (a[i] != b[i])
            return a[i] < b[i] ? 1 : -1;
    }

    return len_a < len_b ? 1 : -1;
}

int lexicographic_sort_reverse(const char* a, const char* b) {
    return lexicographic_sort(b, a);
}

int n_distinct(const char* s) {
    _Bool* has_char = (_Bool*)calloc(26, sizeof(_Bool));

    int len = strlen(s);
    for(int i = 0; i < len; ++i)
        has_char[s[i] - 'a'] = 1;

    int n_distinct = 0;
    for(int i = 0; i < 26; ++i)
        if (has_char[i])
            ++n_distinct;

    free(has_char);
    //printf("n_distinct(%s) = %d\n", s, n_distinct);
    return n_distinct;
}

int sort_by_number_of_distinct_characters(const char* a, const char* b) {
    int n_distinct_a = n_distinct(a);
    int n_distinct_b = n_distinct(b);
    if (n_distinct_a == n_distinct_b)
        return lexicographic_sort(a, b);
    return n_distinct_a < n_distinct_b ? 1 : -1;
}

int sort_by_length(const char* a, const char* b) {
    int len_a = strlen(a);
    int len_b = strlen(b);
    if (len_a == len_b)
        return lexicographic_sort(a, b);
    return len_a < len_b ? 1 : -1;
}

void string_sort(char** arr,const int len,int (*cmp_func)(const char* a, const char* b)){

    // _Bool has_changes = 1;

    // // bubble sort
    // while(has_changes) {
    //     has_changes = 0;

    //     for(int i = 0; i < len - 1; ++i) {

    //         char* a = arr[i];
    //         char* b = arr[i + 1];

    //         // if a and b are in the wrong order then swap them
    //         if (cmp_func(a, b) == -1) {
    //             has_changes = 1;
    //             arr[i] = b;
    //             arr[i + 1] = a;
    //         }
    //     }
    // }

    if (len == 1)
        return;

    int len_first_half = len / 2;
    int len_second_half = len - len_first_half;

    char** first_half = arr;
    char** second_half = arr + len_first_half;

    string_sort(first_half, len_first_half, cmp_func);
    string_sort(second_half, len_second_half, cmp_func);

    char** merged = (char**)malloc(len * sizeof(char*));

    int first_i = 0;
    int second_i = 0;
    for(int i = 0; i < len; ++i) {
        int sort_order = 0;
        if (first_i == len_first_half)
            sort_order = -1;
        else if (second_i == len_second_half)
            sort_order = 1;
        else
            sort_order = cmp_func(first_half[first_i], second_half[second_i]);

        if (sort_order == 1) {
            merged[i] = first_half[first_i];
            ++first_i;
        }

        if (sort_order == -1) {
            merged[i] = second_half[second_i];
            ++second_i;
        }
    }

    for(int i = 0; i < len; ++i)
        arr[i] = merged[i];

    free(merged);
}


int main()
{
    int n;
    scanf("%d", &n);

    char** arr;
	arr = (char**)malloc(n * sizeof(char*));

    for(int i = 0; i < n; i++){
        *(arr + i) = malloc(1024 * sizeof(char));
        scanf("%s", *(arr + i));
        *(arr + i) = realloc(*(arr + i), strlen(*(arr + i)) + 1);
    }

    string_sort(arr, n, lexicographic_sort);
    for(int i = 0; i < n; i++)
        printf("%s\n", arr[i]);
    printf("\n");

    string_sort(arr, n, lexicographic_sort_reverse);
    for(int i = 0; i < n; i++)
        printf("%s\n", arr[i]);
    printf("\n");

    string_sort(arr, n, sort_by_length);
    for(int i = 0; i < n; i++)
        printf("%s\n", arr[i]);
    printf("\n");

    string_sort(arr, n, sort_by_number_of_distinct_characters);
    for(int i = 0; i < n; i++)
        printf("%s\n", arr[i]);
    printf("\n");
}