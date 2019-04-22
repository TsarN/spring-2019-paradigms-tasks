#include "tsqueue.h"

void threadsafe_queue_init(ThreadsafeQueue *q) {
    queue_init(&q->q);
    q->mu = PTHREAD_MUTEX_INITIALIZER;
    q->cv = PTHREAD_COND_INITIALIZER;
    q->size = 0;
}

void threadsafe_queue_destroy(ThreadsafeQueue *q) {
    queue_destroy(&q->q);
}

void threadsafe_queue_push(ThreadsafeQueue *q, void *data) {
    pthread_mutex_lock(&q->mu);
    q->size++;
    queue_push(&q->q, data);
    pthread_cond_broadcast(&q->cv);
    pthread_mutex_unlock(&q->mu);
}

void *threadsafe_queue_wait_and_pop(ThreadsafeQueue *q) {
    pthread_mutex_lock(&q->mu);
    while (!q->size) {
        pthread_cond_wait(&q->cv, &q->mu);
    }
    q->size--;
    void *ret = queue_pop(&q->q);
    pthread_mutex_unlock(&q->mu);
    return ret;
}
