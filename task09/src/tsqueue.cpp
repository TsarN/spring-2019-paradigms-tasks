#include "tsqueue.h"

void threadsafe_queue_init(ThreadsafeQueue *q) {
    queue_init(&q->q);
    pthread_mutex_init(&q->mu, NULL);
    pthread_cond_init(&q->cv, NULL);
}

void threadsafe_queue_destroy(ThreadsafeQueue *q) {
    queue_destroy(&q->q);
}

void threadsafe_queue_push(ThreadsafeQueue *q, void *data) {
    pthread_mutex_lock(&q->mu);
    queue_push(&q->q, data);
    pthread_cond_signal(&q->cv);
    pthread_mutex_unlock(&q->mu);
}

void *threadsafe_queue_wait_and_pop(ThreadsafeQueue *q) {
    pthread_mutex_lock(&q->mu);
    while (queue_empty(&q->q)) {
        pthread_cond_wait(&q->cv, &q->mu);
    }
    void *ret = queue_pop(&q->q);
    pthread_mutex_unlock(&q->mu);
    return ret;
}
