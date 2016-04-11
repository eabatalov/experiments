#include <iostream>
#include <cstdio>
#include <cstring>
#include <utility>
#include <cassert>
#include <cstdint>
#include <memory>

void strrev(char *str)
{
    size_t len = strlen(str); // ignores '\0'
    char *left = str;
    char *right = str + len - 1;
    while(left < right)
    {
        std::swap(*left, *right);
        ++left;
        --right;
    }
}

static void test_strrev_empty()
{
    char str[] = "";
    strrev(str);
    assert(strcmp(str, "") == 0);
}

static void test_strrev_single_char()
{
    char str[] = "t";
    strrev(str);
    assert(strcmp(str, "t") == 0);
}

static void test_strrev_odd_chars()
{
    char str[] = "123456789";
    strrev(str);
    assert(strcmp(str, "987654321") == 0);
}

static void test_strrev_even_chars()
{
    char str[] = "1234567890";
    strrev(str);
    assert(strcmp(str, "0987654321") == 0);
}

static void test_strrev_char_subsets()
{
    char str[] = "a1V%\t\5";
    strrev(str);
    assert(strcmp(str, "\5\t%V1a") == 0);
}

static void test_strrev()
{
    test_strrev_empty();
    test_strrev_single_char();
    test_strrev_odd_chars();
    test_strrev_even_chars();
    test_strrev_char_subsets();
}

const size_t N = 5;
typedef uint32_t img_t[N][N];
void img_rot_90(img_t &matrix)
{
    for(size_t i = 0; i < N; ++i)
        for(size_t j = i; j < N; ++j)
        {
            std::swap(matrix[i][j], matrix[j][i]);
        }
}

bool img_equal(img_t &img1, img_t &img2)
{
    for(size_t i = 0; i < N; ++i)
        for(size_t j = 0; j < N; ++j)
        {
            if (img1[i][j] != img2[i][j])
                return false;
        }
    return true;
}

static void test_img_rot_90()
{
    img_t matrix = {
        { 0, 0, 0, 0, 0 },
        { 0, 1, 0, 0, 0 },
        { 0, 1, 0, 0, 0 },
        { 0, 1, 0, 0, 0 },
        { 0, 0, 1, 0, 0 }
    };
    img_t res_matrix = {
        { 0, 0, 0, 0, 0 },
        { 0, 1, 1, 1, 0 },
        { 0, 0, 0, 0, 1 },
        { 0, 0, 0, 0, 0 },
        { 0, 0, 0, 0, 0 }
    };
    img_rot_90(matrix);
    assert(img_equal(matrix, res_matrix));
}

void print_array(int *ar, size_t size)
{
    for(size_t i = 0; i < size; ++i)
        printf("%d ", ar[i]);
    printf("\n");
}

void quick_sort(int* const ar, size_t const size)
{
    if (size <= 1)
        return;

    int pivot = ar[size - 1];
    int *left = ar;
    int *right = ar + (size - 1);
    while(left != right)
    {
        if (*left <= pivot)
            ++left;
        else if (*right > pivot)
            --right;
        else
            std::swap(*left, *right);
    }
    if (*left > pivot)
        --left;
    else
        ++right;

    // Check 2 cases when ar contains equal elements
    const size_t left_size = (size_t)(left - ar + 1);
    if (left_size < size)
        quick_sort(ar, left_size);
    const size_t right_size = (size_t)((ar + size) - right);
    if (right_size < size)
        quick_sort(right, right_size);
}

void insertion_sort(int* const ar, const size_t size)
{
    for(size_t last_not_sorted_ix = 1; last_not_sorted_ix < size;
            ++last_not_sorted_ix)
    {
        for(size_t i = last_not_sorted_ix; i > 0; --i)
        {
            if (ar[i - 1] > ar[i])
                std::swap(ar[i - 1], ar[i]);
            else
                break;
        }
    }
}

static void test_arrays_equal(int ar1[], int ar2[], size_t size)
{
    for(size_t i = 0; i != size; ++i)
        assert(ar1[i] == ar2[i]);
}

static void test_quick_sort()
{
    const int N = 10;
    int ar_qck[N] = { 5, 3, 2, 6, 7, 1, 34, 2, 10, 1 };
    int ar_ins[N];
    std::copy(ar_qck, ar_qck + N, ar_ins);

    quick_sort(ar_qck, N);
    insertion_sort(ar_ins, N);
    test_arrays_equal(ar_qck, ar_ins, N);
    // TODO
    // test arrays of equal numbers
    // test already sorted arrays
    // test arrays of zero size
    // test arrays on size 1
    // test with short array and all its permutations generator - brute force
}

static void count_sort(int *ar, size_t size)
{
    int max = 0;
    for(size_t i = 0; i != size; ++i)
        max = std::max(max, ar[i]);

    size_t *val_ix = new size_t[max + 1];
    std::fill(val_ix, val_ix + max + 1, 0);
    for(size_t i = 0; i != size; ++i)
        val_ix[ar[i]]++;
    size_t ix = 0;
    for(int i = 0; i <= max; ++i)
    {
        size_t val_count = val_ix[i];
        val_ix[i] = ix;
        ix += val_count;
    }

    int *sorted = new int[size];
    for(size_t i = 0; i != size; ++i)
        sorted[val_ix[ar[i]]++] = ar[i];
    std::copy(sorted, sorted + size, ar);

    delete[] val_ix;
    delete[] sorted;
}

static void test_count_sort()
{
    const int N = 15;
    int ar_count[N] = { 4, 5, 3, 2, 6, 1, 8, 5, 2, 12, 5454, 12, 3, 3, 4 };
    int ar_ins[N];
    std::copy(ar_count, ar_count + N, ar_ins);
    count_sort(ar_count, N);
    insertion_sort(ar_ins, N);
    print_array(ar_count, N);
    print_array(ar_ins, N);
    test_arrays_equal(ar_count, ar_ins, N);
}

static void kv_count_sort(int *keys, int* values, size_t size)
{
    int max = 0;
    for(size_t i = 0; i != size; ++i)
        max = std::max(max, keys[i]);

    size_t *val_ix = new size_t[max + 1];
    std::fill(val_ix, val_ix + max + 1, 0);
    for(size_t i = 0; i != size; ++i)
        val_ix[keys[i]]++;
    size_t ix = 0;
    for(int i = 0; i <= max; ++i)
    {
        size_t val_count = val_ix[i];
        val_ix[i] = ix;
        ix += val_count;
    }

    int *sorted = new int[size];
    for(size_t i = 0; i != size; ++i)
        sorted[val_ix[keys[i]]++] = values[i];
    std::copy(sorted, sorted + size, values);

    delete[] val_ix;
    delete[] sorted;
}

#define get_bit(number, bit) (((number) >> bit) & 0x1)

void lsb_radix_sort(int *ar, size_t size)
{
    const unsigned short bits_cnt = sizeof(*ar) * 8;
    int *bit_keys = new int[size];
    for(size_t bit = 0; bit < bits_cnt; ++bit)
    {
        for(size_t i = 0; i < size; ++i)
            bit_keys[i] = get_bit(ar[i], bit);
        kv_count_sort(bit_keys, ar, size);
    }
    delete[] bit_keys;
}

void test_lsb_radix_sort()
{
    const int N = 15;
    int ar_count[N] = { 4, 5, 3, 2, 6, 1, 8, 5, 2, 12, 5454, 12, 3, 3, 4 };
    int ar_ins[N];
    std::copy(ar_count, ar_count + N, ar_ins);
    lsb_radix_sort(ar_count, N);
    insertion_sort(ar_ins, N);
    print_array(ar_count, N);
    print_array(ar_ins, N);
    test_arrays_equal(ar_count, ar_ins, N);
}

int main() {
    test_strrev();
    test_img_rot_90();
    test_quick_sort();
    test_count_sort();
    test_lsb_radix_sort();
    return 0;
}
