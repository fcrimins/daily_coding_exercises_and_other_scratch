#include <stdio.h>
#include <stdlib.h>

/*
 * This stores the total number of books in each shelf.
 */
int* total_number_of_books;

/*
 * This stores the total number of pages in each book of each shelf.
 * The rows represent the shelves and the columns represent the books.
 */
int** total_number_of_pages;

/**
 * https://www.hackerrank.com/challenges/dynamic-array-in-c/problem
 */
int main()
{
    int total_number_of_shelves;
    scanf("%d", &total_number_of_shelves);

    int total_number_of_queries;
    scanf("%d", &total_number_of_queries);

    total_number_of_books = (int*)malloc(total_number_of_shelves * sizeof(int));
    total_number_of_pages = (int**)malloc(total_number_of_shelves * sizeof(int*));
    for(int i = 0; i < total_number_of_shelves; ++i) {
        total_number_of_books[i] = 0;
    }

    while (total_number_of_queries--) {
        int type_of_query;
        scanf("%d", &type_of_query);

        if (type_of_query == 1) {
            /*
             * Process the query of first type here.
             */
            int x, y;
            scanf("%d %d", &x, &y);

            //printf("x = %d\n", x);
            //printf("y = %d\n", y);
            //printf("*(total_number_of_books + x) = %d\n", *(total_number_of_books + x));
            //printf("total_number_of_books[x] = %d\n", total_number_of_books[x]);
            int n_x = *(total_number_of_books + x);
            //printf("n_x = %d\n", n_x);
            int* shelf_x = *(total_number_of_pages + x);

            /*printf("x = %d, y = %d\n", x, y);
            for(int i = 0; i < n_x; ++i)
                printf("%d, ", shelf_x[i]);
            printf("end\n");*/

            // copy memory or init for the first time
            if (n_x) {
                //printf("realloc: %d\n", n_x + 1);
                *(total_number_of_pages + x) = (int*)realloc(shelf_x, (n_x + 1) * sizeof(int));
            } else {
                //printf("malloc: %d\n", n_x + 1);
                *(total_number_of_pages + x) = (int*)malloc(sizeof(int));
            }

            // assign book with y pages to the end of shelf x
            *(*(total_number_of_pages + x) + n_x) = y;

            // increment number of books on shelf x
            ++(*(total_number_of_books + x));

            //n_x = *(total_number_of_books + x);
            /*for(int i = 0; i < n_x + 1; ++i)
                printf("%d, ", total_number_of_pages[x][i]);
            printf("end\n");*/


        } else if (type_of_query == 2) {
            int x, y;
            scanf("%d %d", &x, &y);
            printf("%d\n", *(*(total_number_of_pages + x) + y));
        } else {
            int x;
            scanf("%d", &x);
            printf("%d\n", *(total_number_of_books + x));
        }
    }

    if (total_number_of_books) {
        free(total_number_of_books);
    }

    for (int i = 0; i < total_number_of_shelves; i++) {
        if (*(total_number_of_pages + i)) {
            free(*(total_number_of_pages + i));
        }
    }

    if (total_number_of_pages) {
        free(total_number_of_pages);
    }

    return 0;
}