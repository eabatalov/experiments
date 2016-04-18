#include <limits>
#include <iostream>
#include <cassert>
#include <cstring>
#include <cstddef>
#include <unordered_set>

// Code is written with hackton quality
// to quickly explore the main concepts
// I am interested in

#define MB(mbs) ((mbs) << 20)

typedef size_t block_ix_t;
static const block_ix_t INVALID_BLOCK_IX =
    std::numeric_limits<block_ix_t>::max();

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

#define BLOCKS_COUNT_FOR_DATA(data_bytes_size, block_size) \
    (((data_bytes_size) + ((block_size) - 1)) / (block_size))
#define DATA_BLOCK_IX(data_offset, block_size) \
    ((data_offset) / (block_size))

struct block_allocator
{
    block_allocator(block_dev_t &bdev, bool reset=true)
        : bdev_(bdev)
    {
        assert(bdev.block_size() > sizeof(size_t));

        char block[bdev.block_size()];
        if (reset) {
            // Reinitialize "Filesystem"
            const size_t metadata_size_in_bytes =
                // Block align metadata of fixed size
                // for free list random access simplification
                BLOCKS_COUNT_FOR_DATA(
                    sizeof(free_blocks_count_), // length of free list
                    bdev.block_size()
                ) * bdev.block_size()
                + bdev.blocks_count() * sizeof(size_t); // free list of blocks
            const size_t metadata_size_in_blocks =
                BLOCKS_COUNT_FOR_DATA(metadata_size_in_bytes, bdev.block_size());
            const size_t user_blocks_count = bdev.blocks_count() - metadata_size_in_blocks;
            const size_t free_list_last_block = metadata_size_in_blocks + user_blocks_count;
            assert(metadata_size_in_blocks < bdev.blocks_count());

            memset(block, 0, sizeof(block));
            ((size_t*)block)[0] = user_blocks_count; // initial length of free list
            bdev.write_block(0, block);

            free_list_entry_t *block_it = (free_list_entry_t*)block;
            free_list_entry_t * const block_end =
                (free_list_entry_t*)(block + bdev.block_size());
            size_t free_list_block_ix = 1;
            free_list_entry_t free_block_ix = metadata_size_in_blocks;

            while(free_block_ix != free_list_last_block)
            {
                // less because block_size() may not be sizeof(size_t) aligned
                while((block_it < block_end) && (free_block_ix != free_list_last_block))
                {
                    *block_it = free_block_ix;
                    ++block_it;
                    ++free_block_ix;
                }
                bdev.write_block(free_list_block_ix, block);
                block_it = (free_list_entry_t*)block;
                free_list_block_ix++;
            }
        }

        // Read "filesystem" state
        bdev.read_block(0, block);
        free_blocks_count_ = ((size_t*)block)[0];
    }

    size_t alloc_block()
    {
        char block[bdev_.block_size()];
        free_list_entry_t *fle = (free_list_entry_t*)block;
        set_free_blocks_count(free_blocks_count_ - 1, block);
        const size_t allocated_block_free_list_ix = free_blocks_count_;
        const size_t fle_block_ix = DATA_BLOCK_IX(
            allocated_block_free_list_ix * sizeof(free_list_entry_t), bdev_.block_size()
        );
        bdev_.read_block(1 + fle_block_ix, block);
        const size_t fle_ix =
            allocated_block_free_list_ix % (bdev_.block_size() / sizeof(free_list_entry_t));
        return fle[fle_ix];
    }

    void free_block(size_t block_ix)
    {
        char block[bdev_.block_size()];
        free_list_entry_t *fle = (free_list_entry_t*)block;
        const size_t freed_block_free_list_ix = free_blocks_count_;
        const size_t fle_block_ix = DATA_BLOCK_IX(
            freed_block_free_list_ix * sizeof(free_list_entry_t), bdev_.block_size()
        );
        const size_t fle_ix =
            freed_block_free_list_ix % (bdev_.block_size() / sizeof(free_list_entry_t));
        bdev_.read_block(1 + fle_block_ix, block);
        fle[fle_ix] = block_ix;
        bdev_.write_block(1 + fle_block_ix, block);
        set_free_blocks_count(free_blocks_count_ + 1, block);
    }

    bool has_free_block()
    {
        return free_blocks_count_ > 0;
    }

private:
    typedef size_t free_list_entry_t;
    size_t free_blocks_count_;
    block_dev_t &bdev_;

    void set_free_blocks_count(size_t new_value, char* block_buf)
    {
        size_t *free_blocks_count_bd = (size_t*)block_buf;
        free_blocks_count_ = new_value;
        *free_blocks_count_bd = new_value;
        bdev_.write_block(0, block_buf);
    }
};

static void test_block_allocator()
{
    const size_t BLOCK_SIZE = 64;
    const size_t BDEV_SIZE = MB(5ULL);
    block_dev_t bdev(BDEV_SIZE / BLOCK_SIZE, BLOCK_SIZE);
    block_allocator bdev_alloc(bdev, true);

    std::unordered_set<size_t> allocated_blocks;
    while(bdev_alloc.has_free_block())
    {
        size_t allocated_block = bdev_alloc.alloc_block();
        bool inserted = allocated_blocks.insert(allocated_block).second;
        assert(inserted);
    }

    // Now we'll deallocate blocks in some random order
    for(size_t block : allocated_blocks)
    {
        bdev_alloc.free_block(block);
    }
    allocated_blocks.clear();

    // And then we allocate all the blocks again from
    // full but not newly created bdev allocator
    while(bdev_alloc.has_free_block())
    {
        size_t allocated_block = bdev_alloc.alloc_block();
        bool inserted = allocated_blocks.insert(allocated_block).second;
        assert(inserted);
    }

    // Cool. Now we can check that block allocator
    // restores its state form block dev
    block_allocator bdev_alloc2(bdev, false);
    assert(!bdev_alloc2.has_free_block());
    std::unordered_set<size_t> free_blocks;
    for(size_t i = 0; i < 3; ++i)
    {
        size_t freed_block = *allocated_blocks.begin();
        allocated_blocks.erase(freed_block);
        free_blocks.insert(freed_block);
        bdev_alloc2.free_block(freed_block);
    }

    while(!free_blocks.empty())
    {
        size_t allocated_block = bdev_alloc2.alloc_block();
        assert(free_blocks.find(allocated_block) != free_blocks.end());
        free_blocks.erase(allocated_block);
    }
    assert(!bdev_alloc2.has_free_block());
}

template<class KEY>
struct bplus_tree_node_t
{
    bplus_tree_node_t(block_dev_t &bdev, block_ix_t node_block_ix, bool is_new=false)
        : bdev_(bdev)
        , block_ix_(node_block_ix)
    {
        node_block_ = (pod_bplus_tree_node_t*)new char[bdev_.block_size()];
        if (is_new)
        {
            node_block_->is_leaf_ = false;
            node_block_->parent_block_ix_ = INVALID_BLOCK_IX;
            node_block_->keys_count_ = 0;
            bdev_.write_block(block_ix_, (void*)node_block_);
        } else
        {
            bdev_.read_block(block_ix_, (void*)node_block_);
        }
        assert(max_entries_count() >= 2);
    }

    bool get_is_leaf() const
    {
        return node_block_->is_leaf_;
    }

    void set_is_leaf(bool is_leaf)
    {
        node_block_->is_leaf_ = is_leaf;
        bdev_.write_block(block_ix_, (void*)node_block_);
    }

    block_ix_t get_parent_block_ix() const
    {
        return node_block_->parent_block_ix_;
    }

    void set_parent_block_ix(block_ix_t parent_block_ix)
    {
        node_block_.parent_block_ix_ = parent_block_ix;
        bdev_.write_block(block_ix_, (void*)node_block_);
    }

    block_ix_t get_next_inorder_block_ix()
    {
        assert(node_block_->is_leaf_);
        return (entries_begin() + max_entries_count() - 1)->block_ix_;
    }

    void set_next_inorder_block_ix(block_ix_t block_ix)
    {
        assert(node_block_->is_leaf_);
        (entries_begin() + max_entries_count() - 1)->block_ix_ = block_ix;
        bdev_.write_block(block_ix_, (void*)node_block_);
    }

    unsigned short get_free_entries_count() const
    {
        return max_entries_count() - node_block_->keys_count_
            - (node_block_->is_leaf_ ? 1 : 0);
    }

    block_ix_t get_key_block_ix(const KEY &key)
    {
        // As keys are sorted it can be implemented as binary search
        for(block_ix_and_key_t *bixandk = entries_begin();
                bixandk != entries_end(); ++bixandk)
        {
            if (key == bixandk->key_)
                return bixandk->block_ix_;
        }
        return INVALID_BLOCK_IX;
    }

    bool add_entry(const KEY &key, block_ix_t block_ix)
    {
        assert(get_key_block_ix(key) == INVALID_BLOCK_IX);
        if (get_free_entries_count() == 0)
            return false;

        size_t insert_ix = 0;
        for(block_ix_and_key_t *bixandk = entries_begin();
            bixandk != entries_end(); ++bixandk)
        {
            if (bixandk->key_ > key)
            {
                insert_ix = bixandk - entries_begin();
                break;
            }
            ++insert_ix;
        }
        block_ix_and_key_t *inserted_bixandk =
            entries_begin() + insert_ix;

        memmove(
            (void*)(inserted_bixandk + 1),
            (void*)inserted_bixandk,
            (entries_end() - inserted_bixandk) * sizeof(*inserted_bixandk)
        );
        inserted_bixandk->key_ = key;
        inserted_bixandk->block_ix_ = block_ix;
        set_keys_count(node_block_->keys_count_ + 1);
        return true;
    }

    bool delete_entry(const KEY &key)
    {
        if (get_key_block_ix(key) == INVALID_BLOCK_IX)
            return false;

        block_ix_and_key_t *bixandk_to_delete = nullptr;
        for(block_ix_and_key_t *bixandk = entries_begin();
            bixandk != entries_end(); ++bixandk)
        {
            if (bixandk->key_ == key)
            {
                bixandk_to_delete = bixandk;
                break;
            }
        }
        memmove(
            (void*)(bixandk_to_delete),
            (void*)(bixandk_to_delete + 1),
            (entries_end() - bixandk_to_delete - 1) * sizeof(*bixandk_to_delete)
        );
        set_keys_count(node_block_->keys_count_ - 1);
        return true;
    }

    ~bplus_tree_node_t()
    {
        delete[] node_block_;
    }

private:
    struct pod_bplus_tree_node_t;
    block_dev_t &bdev_;
    pod_bplus_tree_node_t *node_block_;
    block_ix_t block_ix_;

    struct block_ix_and_key_t
    {
        KEY key_;
        block_ix_t block_ix_;
    };

    struct pod_bplus_tree_node_t
    {
        bool is_leaf_;
        block_ix_t parent_block_ix_;
        unsigned short keys_count_;
        block_ix_and_key_t bixandk_[];
    };

    block_ix_and_key_t* entries_begin()
    {
        return (block_ix_and_key_t*)node_block_->bixandk_;
    }

    block_ix_and_key_t* entries_end()
    {
        return ((block_ix_and_key_t*)node_block_->bixandk_) + node_block_->keys_count_;
    }

    size_t max_entries_count() const
    {
        const size_t header_size =
            offsetof(pod_bplus_tree_node_t, bixandk_);
        return (bdev_.block_size() - header_size)
            / sizeof(block_ix_and_key_t);
    }

    void set_keys_count(unsigned short new_keys_count)
    {
        node_block_->keys_count_ = new_keys_count;
        bdev_.write_block(block_ix_, (void*)node_block_);
    }
};

void test_bplus_tree_node()
{
    typedef unsigned short key_t;
    const size_t BLOCK_SIZE = 256;
    const size_t BDEV_SIZE = MB(1ULL);
    block_dev_t bdev(BDEV_SIZE / BLOCK_SIZE, BLOCK_SIZE);
    bplus_tree_node_t<key_t> node(bdev, 0, true);
    node.set_is_leaf(true);
    node.set_next_inorder_block_ix(777);

    const size_t node_capacity = node.get_free_entries_count();
    std::unordered_set<key_t> test_keys;
    for(key_t i = 0; i < node_capacity; ++i)
    {
        test_keys.insert(i);
    }
    // pseudo random order
    for(key_t key : test_keys)
    {
        assert(node.add_entry(key, key));
    }
    assert(node.get_free_entries_count() == 0);

    for(key_t key : test_keys)
    {
        assert(node.get_key_block_ix(key) == key);
    }

    // test node state persistancy
    bplus_tree_node_t<unsigned short> node1(bdev, 0, false);
    assert(node.get_key_block_ix(*test_keys.begin()) ==
        node1.get_key_block_ix(*test_keys.begin()));
    assert(node.get_parent_block_ix() == node1.get_parent_block_ix());
    assert(node.get_free_entries_count() == node1.get_free_entries_count());
    assert(node.get_is_leaf() == node1.get_is_leaf());
    assert(node.get_next_inorder_block_ix() == node1.get_next_inorder_block_ix());

    // check remove
    size_t free_entries_count = node.get_free_entries_count();
    for(key_t key : test_keys)
    {
        assert(node.delete_entry(key));
        free_entries_count++;
        assert(node.get_free_entries_count() == free_entries_count);
    }
    assert(node.get_next_inorder_block_ix() == 777);
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
    block_dev_t bdev(MB(2ULL) / BLOCK_SIZE, BLOCK_SIZE);

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
    test_block_allocator();
    test_bplus_tree_node();
    test_bplus_tree();
    return 0;
}
