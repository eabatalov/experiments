#include <cassert>
#include <cstdio>
#include <algorithm>

// arl and arr should go one after another in memory (continuous)
void merge_arrays(int *arl, size_t arl_size, int *arr, size_t arr_size)
{
    const size_t arm_size = arl_size + arr_size;
    int *arm = new int[arm_size];
    // 2 way merge
    size_t arm_ix = 0;
    size_t arl_ix = 0;
    size_t arr_ix = 0;
    while(arm_ix < arm_size)
    {
        if ((arr_ix != arr_size) && (arl_ix != arl_size))
        {
            if (arl[arl_ix] <= arr[arr_ix])
            {
                arm[arm_ix] = arl[arl_ix];
                arl_ix++;
            } else
            {
                arm[arm_ix] = arr[arr_ix];
                ++arr_ix;
            }
        } else if (arl_ix == arl_size)
        {
            arm[arm_ix] = arr[arr_ix];
            ++arr_ix;
        } else if (arr_ix == arr_size)
        {
            arm[arm_ix] = arl[arl_ix];
            arl_ix++;
        }
        ++arm_ix;
    }
    assert((arl_ix == arl_size) && (arr_ix == arr_size));

    std::copy(arm, arm + arm_size, arl);
    delete[] arm;
}

void merge_sort(int *ar, size_t size)
{
    if (size < 2)
        return;

    // [0, mid_ix), [mid_ix, size - 1]
    size_t mid_ix = size / 2;
    int *const arl_begin = ar;
    const size_t arl_size = mid_ix;
    int *const arr_begin = ar + mid_ix;
    const size_t arr_size = size - mid_ix;

    merge_sort(arl_begin, arl_size);
    merge_sort(arr_begin, arr_size);
    merge_arrays(arl_begin, arl_size, arr_begin, arr_size);
}

static void test_merge_sort()
{
    const int N = 11;
    int src_ar[N] = { 3, 2, 1, 4, 5, 6, 10, 1, 22, 11, -5 };
    int sorted_ar[N];
    std::copy(src_ar, src_ar + N, sorted_ar);
    merge_sort(sorted_ar, N);
    for(size_t i = 0; i < N; ++i)
        printf("%d ", sorted_ar[i]);
    printf("\n");
    for(size_t i = 0; i < (N - 1); ++i)
        assert(sorted_ar[i] <= sorted_ar[i + 1]);
}

int main()
{
    test_merge_sort();	
    return 0;
}
