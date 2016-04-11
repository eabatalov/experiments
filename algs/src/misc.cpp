#include <cstdint>
#include <utility>
#include <cassert>
#include <vector>
#include <string>
#include <functional>
#include <deque>
#include <cstdlib>
#include <ctime>
#include <algorithm>
#include <iostream>

template<class T>
struct list_node_t
{
    T& value()
    {
        return value_;
    }

    list_node_t *next()
    {
        return next_;
    }

    // prev is null if this is list head
    void remove(list_node_t *prev)
    {
        if (prev)
            prev->next_ = next_;
        next_ = nullptr;
    }

    // prev or next can be nullptr is this is head or tail
    void insert(list_node_t *prev, list_node_t *next)
    {
        assert(!next_);
        assert(next || prev);
        if (prev)
        {
            assert(prev->next_ == next);
            prev->next_ = this;
        }
        if (next)
        {
            next_ = next;
        }
    }

    void link(list_node_t *next)
    {
        assert(!next_);
        next_ = next;
    }

    void unlink()
    {
        assert(next_);
        next_ = nullptr;
    }

    static list_node_t *create(T value)
    {
        return new list_node_t(std::move(value));
    }

    static void destroy(list_node_t *node)
    {
        delete node;
    }

private:
    list_node_t(T &&value)
        : value_(std::forward<T>(value))
        , next_(nullptr)
    {}

    T value_;
    list_node_t *next_;
};

template<class T>
void destroy_list(list_node_t<T> *head)
{
    typedef list_node_t<T> node_t;
    node_t *current = head;
    while(current)
    {
        node_t *prev = current;
        current = prev->next();
        node_t::destroy(prev);
    }
}

template<class T>
list_node_t<T>* vector_to_list(const std::vector<T> &vec)
{
    if (vec.size() == 0)
        return nullptr;

    typedef list_node_t<T> node_t;
    node_t *head = node_t::create(vec.front());
    node_t *prev = head;
    for(auto it = vec.begin() + 1; it != vec.end(); ++it)
    {
        node_t *current = node_t::create(*it);
        current->insert(prev, nullptr);
        prev = current;
    }
    return head;
}

static void test_linked_list()
{
    std::vector<int> vec_empty;
    assert(vector_to_list(vec_empty) == nullptr);

    std::vector<std::string> vec_full = {
        "123", "4567890", "777 vipil", "mmmm? kto takoi?"
    };
    auto *list = vector_to_list(vec_full);
    auto *current = list;
    for(size_t i = 0; i != vec_full.size(); ++i)
    {
        assert(vec_full[i] == current->value());
        current = current->next();
    }
    assert(current == nullptr);
    destroy_list(list);
}

// return head to merged list
template<class T>
list_node_t<T>* lists_merge(list_node_t<T> *head1, list_node_t<T> *head2)
{
    typedef list_node_t<T> node_t;
    node_t *merged_head = nullptr;
    node_t *merged_tail = nullptr;
    node_t *cur1 = head1;
    node_t *cur2 = head2;

    if (head1->value() <= head2->value())
    {
        merged_head = head1;
        cur1 = cur1->next();
    } else
    {
        merged_head = head2;
        cur2 = cur2->next();
    }
    merged_tail = merged_head;
    merged_tail->remove(nullptr);

    while(cur1 && cur2)
    {
        node_t *min_node = nullptr;
        if (cur1->value() <= cur2->value())
        {
            min_node = cur1;
            cur1 = cur1->next();
        }
        else
        {
            min_node = cur2;
            cur2 = cur2->next();
        }
        min_node->remove(nullptr);
        merged_tail->link(min_node);
        merged_tail = min_node;
    }

    if (cur1)
    {
        merged_tail->link(cur1);
    } else
    {
        merged_tail->link(cur2);
    }

    return merged_head;
}

static void test_linked_list_merge()
{
    std::vector<int> vec1 = {
        -100, -30, -20, -11, -1, 0, 0, 0, 1, 2, 3, 4, 5, 6, 19, 121
    };
    std::vector<int> vec2 = {
        -1000, -200, -30, -19, -2, 0, 0, 2, 3, 4, 5, 111, 123, 1323, 2000
    };
    auto *list1 = vector_to_list(vec1);
    auto *list2 = vector_to_list(vec2);
    auto *merged_list = lists_merge(list1, list2);
    auto *prev = merged_list;
    auto *current = merged_list->next();
    size_t merged_size = 1;
    while(current)
    {
        assert(prev->value() <= current->value());
        ++merged_size;
        prev = current;
        current = current->next();
    }
    destroy_list(merged_list);
    assert(merged_size == (vec1.size() + vec2.size()));
}

template<class T>
list_node_t<T>* list_middle(list_node_t<T> *head)
{
    typedef list_node_t<T> node_t;
    node_t *current = head;
    node_t *current2x = head->next();
    while(current2x)
    {
        current2x = current2x->next();
        if (current2x)
        {
            current2x = current2x->next();
            current = current->next();
        }
    }
    return current;
}

template<class T>
list_node_t<T>* merge_sort(list_node_t<T> *head)
{
    typedef list_node_t<T> node_t;
    if (!head->next())
        return head; // sorted 1 element list

    // [left_head, left_tail], [right_head, right_tail]
    node_t *left_head = head;
    node_t *left_tail = list_middle(head);
    node_t *right_head = left_tail->next();
    left_tail->unlink();
    left_head = merge_sort(left_head);
    right_head = merge_sort(right_head);
    return lists_merge(left_head, right_head);
}

static void test_linked_list_merge_sort()
{
    srand(time(NULL));
    std::vector<int> not_sorted_vec;
    while(not_sorted_vec.size() < 30)
        not_sorted_vec.push_back(rand());
    std::vector<int> sorted_vec = not_sorted_vec;
    std::sort(sorted_vec.begin(), sorted_vec.end());
    list_node_t<int> *list = vector_to_list(not_sorted_vec);
    list = merge_sort(list);
    size_t cur_ix = 0;
    list_node_t<int> *current = list;
    while(current)
    {
        std::cout << current->value()
            << " " << sorted_vec[cur_ix] << std::endl;
        assert(current->value() == sorted_vec[cur_ix]);
        current = current->next();
        ++cur_ix;
    }
    destroy_list(list);
}

template<class T, class LESS = std::less<T>>
struct min_heap_t
{
    T pop()
    {
        using std::swap;
        T top = std::move(data_.front());
        std::swap(data_.front(), data_.back());
        data_.pop_back();
        heapify_from_top();
        return top;
    }

    void push(T value)
    {
        size_t push_ix = data_.size();
        data_.emplace_back(std::move(value));
        heapify_up_from(push_ix);
    }

    size_t size() const
    {
        return data_.size();
    }

private:
    enum class HEAPIFY_PARENT_RESULT
    {
        DONE_NOTHING,
        SWAPPED_LEFT,
        SWAPPED_RIGHT
    };

    void heapify_from_top()
    {
        HEAPIFY_PARENT_RESULT last_result;
        ssize_t current_ix = 0;
        while(true)
        {
            last_result = heapify_parent(current_ix);
            switch(last_result)
            {
                case HEAPIFY_PARENT_RESULT::SWAPPED_LEFT:
                    current_ix = left_child_ix(current_ix);
                break;
                case HEAPIFY_PARENT_RESULT::SWAPPED_RIGHT:
                    current_ix = right_child_ix(current_ix);
                break;
                case HEAPIFY_PARENT_RESULT::DONE_NOTHING:
                    return;
            }
        }
    }

    void heapify_up_from(ssize_t start_ix)
    {
        ssize_t current_ix = start_ix;
        current_ix = parent_ix(current_ix);
        HEAPIFY_PARENT_RESULT last_result =
            HEAPIFY_PARENT_RESULT::DONE_NOTHING;
        do
        {
            last_result = heapify_parent(current_ix);
            current_ix = parent_ix(current_ix);
        } while(last_result != HEAPIFY_PARENT_RESULT::DONE_NOTHING);
    }

    HEAPIFY_PARENT_RESULT heapify_parent(ssize_t parent_ix)
    {
        using std::swap;
        if (parent_ix == -1)
            return HEAPIFY_PARENT_RESULT::DONE_NOTHING;

        ssize_t left_ix = left_child_ix(parent_ix);
        ssize_t right_ix = right_child_ix(parent_ix);
        if ((left_ix == -1) && (right_ix == -1))
            return HEAPIFY_PARENT_RESULT::DONE_NOTHING;

        T *parent_val = &data_[parent_ix];
        T *left_val = left_ix == -1 ? nullptr : &data_[left_ix];
        T *right_val = right_ix == -1 ? nullptr : &data_[right_ix];

        LESS less;
        if (left_val && right_val)
        {
            bool left_is_less_right = less(*left_val, *right_val);
            T *min_val = left_is_less_right ? left_val : right_val;
            if (less(*parent_val, *min_val))
                return HEAPIFY_PARENT_RESULT::DONE_NOTHING;

            if (left_is_less_right)
            {
                swap(*left_val, *parent_val);
                return HEAPIFY_PARENT_RESULT::SWAPPED_LEFT;
            } else
            {
                swap(*right_val, *parent_val);
                return HEAPIFY_PARENT_RESULT::SWAPPED_RIGHT;
            }
        } else if (left_val)
        {
            if (less(*parent_val, *left_val))
                return HEAPIFY_PARENT_RESULT::DONE_NOTHING;

            std::swap(*parent_val, *left_val);
            return HEAPIFY_PARENT_RESULT::SWAPPED_LEFT;
        } else
        {
            assert(false); // can't be right value without left
        }
    }

    ssize_t left_child_ix(ssize_t parent_ix)
    {
        if ((parent_ix == -1) || (parent_ix >= data_.size()))
            return -1;
        ssize_t left_ix = parent_ix * 2 + 1;
        if (left_ix >= data_.size())
            return -1;
        return left_ix;
    }

    ssize_t right_child_ix(ssize_t parent_ix)
    {
        if ((parent_ix == -1) || (parent_ix >= data_.size()))
            return -1;
        ssize_t right_ix = parent_ix * 2 + 2;
        if (right_ix >= data_.size())
            return -1;
        return right_ix;
    }

    ssize_t parent_ix(ssize_t child_ix)
    {
        if ((child_ix == -1) || (child_ix >= data_.size()))
            return -1;
        if (child_ix == 0)
            return -1;
        return (child_ix - 1) / 2;
    }
    std::deque<T> data_;
};

static void test_heap()
{
    min_heap_t<int, std::greater<int>> heap;
    assert(heap.size() == 0);
    heap.push(10);
    heap.push(0);
    heap.push(20);
    heap.push(0);
    assert(heap.size() == 4);
    assert(heap.pop() == 20);
    assert(heap.pop() == 10);
    assert(heap.pop() == 0);
    assert(heap.size() == 1);
    heap.push(-100);
    heap.push(-40);
    heap.push(-200);
    heap.push(100);
    assert(heap.size() == 5);
    assert(heap.pop() == 100);
    assert(heap.pop() == 0);
    assert(heap.pop() == -40);
    assert(heap.pop() == -100);
    assert(heap.pop() == -200);
    assert(heap.size() == 0);
}

template<class KEY, class VALUE>
struct hashmap_t
{
    static const size_t MAX_LOAD_FACTOR;

    hashmap_t()
        : buckets_count_(MIN_BUCKETS_COUNT)
        , entries_count_(0)
        , buckets_(new bucket_node_t*[buckets_count_])
    {
        for(size_t i = 0; i < buckets_count_; ++i)
            buckets_[i] = nullptr;
    }

    ~hashmap_t()
    {
        delete[] buckets_;
    }

    bool put(const KEY &key, const VALUE &value)
    {
        entry_t new_entry(key, value);
        bucket_node_t *new_bnode =
            bucket_node_t::create(std::move(new_entry));
        bool success = add_bucket_node(new_bnode);
        if (success)
        {
            ++entries_count_;
            rehash_if_needed();
        }
        else
        {
            bucket_node_t::destroy(new_bnode);
        }
        return success;
    }

    VALUE* get(const KEY &key)
    {
        bucket_node_t *bnode = buckets_[bucket_ix(key)];
        while(bnode)
        {
            if (bnode->value().first == key)
                return &bnode->value().second;
            bnode = bnode->next();
        }
        return nullptr;
    }

    size_t size() const
    {
        return entries_count_;
    }

    double load_factor()
    {
        return ((double)entries_count_) / buckets_count_;
    }

private:
    typedef std::pair<const KEY, VALUE> entry_t;
    typedef list_node_t<entry_t> bucket_node_t;
    static const size_t MIN_LOAD_FACTOR;
    static const size_t MIN_BUCKETS_COUNT;

    size_t bucket_ix(const KEY& key)
    {
        return hash_(key) % buckets_count_;
    }

    bool add_bucket_node(bucket_node_t *new_node)
    {
        size_t bix = bucket_ix(new_node->value().first);
        bucket_node_t *bnode = buckets_[bix];
        if (!bnode)
        {
            buckets_[bix] = new_node;
            return true;
        }
        bucket_node_t *pbnode = bnode;
        while(bnode)
        {
            if (bnode->value().first == new_node->value().first)
                return false;
            pbnode = bnode;
            bnode = bnode->next();
        }
        new_node->insert(pbnode, nullptr);
        return true;
    }

    void rehash_if_needed()
    {
        size_t new_buckets_count = buckets_count_;
        if (load_factor() < MIN_LOAD_FACTOR)
        {
            new_buckets_count =
                std::max(MIN_BUCKETS_COUNT, (size_t)(buckets_count_ * 0.75));
        } else if (load_factor() >= MAX_LOAD_FACTOR)
        {
            new_buckets_count *= 2;
        }
        if (new_buckets_count == buckets_count_)
            return;

        std::cout << "rehash: " << buckets_count_
            << "->" << new_buckets_count << std::endl;
        bucket_node_t **old_buckets = buckets_;
        size_t old_buckets_count = buckets_count_;
        buckets_ = new bucket_node_t*[new_buckets_count];
        buckets_count_ = new_buckets_count;
        for(size_t i = 0; i != buckets_count_; ++i)
            buckets_[i] = nullptr;

        for(size_t bix = 0; bix != old_buckets_count; ++bix)
        {
            bucket_node_t *bnode = old_buckets[bix];
            while(bnode)
            {
                bucket_node_t *nbnode = bnode->next();
                bnode->remove(nullptr);
                add_bucket_node(bnode);
                bnode = nbnode;
            }
        }
        delete[] old_buckets;
    }

    std::hash<KEY> hash_;
    size_t buckets_count_;
    size_t entries_count_;
    bucket_node_t **buckets_;
};

template<class KEY, class VALUE>
const size_t hashmap_t<KEY, VALUE>::MAX_LOAD_FACTOR = 5;

template<class KEY, class VALUE>
const size_t hashmap_t<KEY, VALUE>::MIN_LOAD_FACTOR = 1.25;

template<class KEY, class VALUE>
const size_t hashmap_t<KEY, VALUE>::MIN_BUCKETS_COUNT = 12;

static void test_hashmap()
{
    hashmap_t<std::string, std::string> map;
    map.put("2", "Eugene Batalov");
    map.put("1", "Andy Lehenki");
    map.put("3", "Big Miha");
    map.put("ABCdefg", "Maks");
    map.put("Hello World!", "Valera");
    map.put("Google", "Comon!");
    assert(map.size() == 6);
    for(size_t i = 6; i < 1000; ++i)
        assert(map.put(std::to_string(i), std::to_string(i)));
    assert(map.size() == 1000);
    assert(*map.get("1") == "Andy Lehenki");
    assert(*map.get("2") == "Eugene Batalov");
    assert(*map.get("3") == "Big Miha");
    assert(*map.get("500") == "500");
    assert(*map.get("111") == "111");
    assert(*map.get("999") == "999");
    assert(*map.get("333") == "333");
    assert(!map.get("1001"));
    assert(!map.put("333", "123"));
    assert(map.load_factor() <= decltype(map)::MAX_LOAD_FACTOR);
    std::cout << "hashmap load factor: " << map.load_factor() << std::endl;
}

int main()
{
    test_linked_list();
    test_linked_list_merge();
    test_linked_list_merge_sort();
    test_heap();
    test_hashmap();
    return 0;
}
