#include <iostream>
#include <cassert>
#include <cstring>

#define MB(mbs) ((mbs) << 20)

struct block_dev_t
{
    block_dev_t(size_t blocks_cnt, size_t block_size = 512)
        : data_(new char[blocks_cnt * block_size])
        , block_size_(block_size)
        , blocks_cnt_(blocks_cnt)
    {}

    ~block_dev_t()
    {
        delete[] data_;
    }

    void read_block(size_t block_ix, void *block_buf)
    {
        memcpy(block_buf, block_ptr(block_ix), block_size());
    }

    void write_block(size_t block_ix, void *block_buf)
    {
        memcpy(block_ptr(block_ix), block_buf, block_size());
    }

    size_t block_size() const
    {
        return block_size_;
    }

    size_t blocks_count() const
    {
        return blocks_cnt_;
    }
private:
    char *data_;
    size_t block_size_;
    size_t blocks_cnt_;

    void* block_ptr(size_t block_ix) const
    {
        assert(block_ix < blocks_cnt_);
        return static_cast<void*>(data_ + block_ix * block_size());
    }
};

static void test_block_dev()
{
    const size_t BLOCK_SIZE = 1ULL << 10;
    block_dev_t bdev(MB(100ULL) / BLOCK_SIZE, BLOCK_SIZE);
    char *block1 = new char[BLOCK_SIZE];
    char *block2 = new char[BLOCK_SIZE];

    // sequential rw check
    memset(block1, 0xDE, BLOCK_SIZE);
    memset(block2, 0xDE, BLOCK_SIZE);
    for(size_t i = 0; i < bdev.blocks_count(); ++i)
        bdev.write_block(i, block1);
    for(size_t i = 0; i < bdev.blocks_count(); ++i)
    {
        bdev.read_block(i, block2);
        assert(memcmp(block1, block2, BLOCK_SIZE) == 0);
    }

    // random access check
    memset(block1, 0xBF, BLOCK_SIZE);
    bdev.write_block(4, block1);
    bdev.write_block(101, block1);
    bdev.write_block(787, block1);
    bdev.write_block(999, block1);

    bdev.read_block(4, block2);
    assert(memcmp(block1, block2, BLOCK_SIZE) == 0);
    bdev.read_block(101, block2);
    assert(memcmp(block1, block2, BLOCK_SIZE) == 0);
    bdev.read_block(787, block2);
    assert(memcmp(block1, block2, BLOCK_SIZE) == 0);
    bdev.read_block(999, block2);
    assert(memcmp(block1, block2, BLOCK_SIZE) == 0);

    delete[] block1;
    delete[] block2;
}

template<class KEY, class VALUE>
struct bplus_tree_t
{
    bplus_tree_t(block_dev_t &bdev)
        : bdev_(bdev)
    {}

    bool put(const KEY &key)
    {
        // TODO
        return false;
    }

    VALUE* get(const KEY &key)
    {
        // TODO
        return nullptr;
    }

    bool erase(const KEY &key)
    {
        // TODO
        return false;
    }
private:
    block_dev_t &bdev_;
};

void test_bplus_tree()
{
    const size_t BLOCK_SIZE = 512; // 512 bytes block
    block_dev_t bdev(MB(100ULL) / BLOCK_SIZE, BLOCK_SIZE);

    typedef std::pair<int, int> key_t;
    struct value_t
    {
        size_t a;
        size_t b;
        size_t c;
    };

    bplus_tree_t<key_t, value_t> tree(bdev);
    // TODO
}

int main()
{
    test_block_dev();
    test_bplus_tree();
    return 0;
}
