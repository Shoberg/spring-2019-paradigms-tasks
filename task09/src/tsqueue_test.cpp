#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "tsqueue.h"
#include "doctest.h"

const int ELEMENTS_PER_THREAD = 1000 * 1000;
const int REPEATS = 3;

TEST_CASE("ThreadsafeQueue works like Queue in a single thread") {
    ThreadsafeQueue q;
    threadsafe_queue_init(&q);

    int a = 0, b = 0, c = 0;

    threadsafe_queue_push(&q, &a);
    threadsafe_queue_push(&q, &b);
    CHECK(threadsafe_queue_wait_and_pop(&q) == &a);
    CHECK(threadsafe_queue_wait_and_pop(&q) == &b);

    threadsafe_queue_push(&q, &c);
    threadsafe_queue_push(&q, &b);
    CHECK(threadsafe_queue_wait_and_pop(&q) == &c);
    CHECK(threadsafe_queue_wait_and_pop(&q) == &b);

    threadsafe_queue_destroy(&q);
}

TEST_CASE("ThreadsafeQueue multithreaded ping-pong") {
    ThreadsafeQueue qs[2];
    threadsafe_queue_init(&qs[0]);
    threadsafe_queue_init(&qs[1]);

    // `PING_PONGS` раз должно произойти следующее:
    // 0. Создаются два потока: `pinger` и `ponger`
    //    (независимые от основного потока теста).
    // 1. Поток `pinger` отправляет `qs[0]` потоку `ponger`
    //    указатель на локальную переменную типа `int`.
    // 2. Поток `ponger` увеличивает полученную переменную на
    //    единицу и отправляет результат обратно через `qs[1]`.
    // 3. Поток `pinger` проверяет, что пришёл правильный адрес
    //    и что локальная переменная была увеличена.
    const int PING_PONGS = 100;

    auto pinger = [](void *_qs) -> void * {
        ThreadsafeQueue *qs = static_cast<ThreadsafeQueue *>(_qs);
        // TODO
        static_cast<void>(qs);  // Используем переменную как-нибудь.
        static_cast<void>(PING_PONGS);  // Используем переменную как-нибудь.
        return nullptr;
    };

    // TODO

    pthread_t t1, t2;
    REQUIRE(pthread_create(&t1, nullptr, pinger, qs) == 0);
    // TODO
    static_cast<void>(t2);
    REQUIRE(pthread_join(t1, nullptr) == 0);

    threadsafe_queue_destroy(&qs[1]);
    threadsafe_queue_destroy(&qs[0]);
}

void *producer(void *_q) {
    ThreadsafeQueue *q = static_cast<ThreadsafeQueue *>(_q);
    for (int i = 0; i < ELEMENTS_PER_THREAD; i++) {
        threadsafe_queue_push(q, nullptr);
    }
    return nullptr;
}

void *consumer(void *_q) {
    ThreadsafeQueue *q = static_cast<ThreadsafeQueue *>(_q);
    for (int i = 0; i < ELEMENTS_PER_THREAD; i++) {
        REQUIRE(threadsafe_queue_wait_and_pop(q) == nullptr);
    }
    return nullptr;
};

TEST_CASE("ThreadsafeQueue pushes from multiple threads") {
    ThreadsafeQueue q;
    threadsafe_queue_init(&q);

    for (int repeat = 0; repeat < REPEATS; repeat++) {
        pthread_t t1, t2;
        REQUIRE(pthread_create(&t1, nullptr, producer, &q) == 0);
        REQUIRE(pthread_create(&t2, nullptr, producer, &q) == 0);
        REQUIRE(pthread_join(t2, nullptr) == 0);
        REQUIRE(pthread_join(t1, nullptr) == 0);

        for (int i = 0; i < 2 * ELEMENTS_PER_THREAD; i++) {
            REQUIRE(threadsafe_queue_wait_and_pop(&q) == nullptr);
        }
    }

    threadsafe_queue_destroy(&q);
}

TEST_CASE("ThreadsafeQueue pops from multiple threads") {
    ThreadsafeQueue q;
    threadsafe_queue_init(&q);

    for (int repeat = 0; repeat < REPEATS; repeat++) {
        for (int i = 0; i < 2 * ELEMENTS_PER_THREAD; i++) {
            threadsafe_queue_push(&q, nullptr);
        }

        pthread_t t1, t2;
        REQUIRE(pthread_create(&t1, nullptr, consumer, &q) == 0);
        REQUIRE(pthread_create(&t2, nullptr, consumer, &q) == 0);
        REQUIRE(pthread_join(t2, nullptr) == 0);
        REQUIRE(pthread_join(t1, nullptr) == 0);
    }

    threadsafe_queue_destroy(&q);
}

TEST_CASE("ThreadsafeQueue pushes and pops from multiple threads") {
    ThreadsafeQueue q;
    threadsafe_queue_init(&q);

    const int THREADS = 5;

    for (int repeat = 0; repeat < REPEATS; repeat++) {
        pthread_t prods[THREADS], cons[THREADS];
        for (int i = 0; i < THREADS; i++) {
            REQUIRE(pthread_create(&prods[i], nullptr, producer, &q) == 0);
            REQUIRE(pthread_create(&cons[i], nullptr, consumer, &q) == 0);
        }
        for (int i = THREADS - 1; i >= 0; i--) {
            REQUIRE(pthread_join(prods[i], nullptr) == 0);
            REQUIRE(pthread_join(cons[i], nullptr) == 0);
        }
    }

    threadsafe_queue_destroy(&q);
}
