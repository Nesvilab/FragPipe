/*
 * This file is part of FragPipe.
 *
 * FragPipe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * FragPipe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package org.nesvilab.fragpipe.api;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.greenrobot.eventbus.EventBus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Bus {
  private static final Logger log = LoggerFactory.getLogger(Bus.class);
  private static final ExecutorService threadPool = Executors.newCachedThreadPool(r -> {
    final Thread t = Executors.defaultThreadFactory().newThread(r);
    t.setDaemon(true); // unused cached threads are only removed after 60s, non daemon threads will keep the JVM from shutting down
    return t;
  });
  private static final EventBus b;

  static {
    b = EventBus.builder()
        .executorService(threadPool)
        .logNoSubscriberMessages(false)
        .build();
  }

  public static void clearCaches() {
    EventBus.clearCaches();
  }

  public static void register(Object subscriber) {
    b.register(subscriber);
  }

  public static void registerQuietly(Object subscriber) {
    for (Method m : subscriber.getClass().getMethods()) {
      for (Annotation a : m.getDeclaredAnnotations()) {
        Class<? extends Annotation> aClass = a.annotationType();
        if (org.greenrobot.eventbus.Subscribe.class.equals(aClass)) {
          b.register(subscriber);
          return;
        }
      }
    }
    log.debug("Message for developers. No @Subscribe annotations found on bus subscriber {}", subscriber.getClass().getCanonicalName());
  }

  public static boolean isRegistered(Object subscriber) {
    return b.isRegistered(subscriber);
  }

  public static void unregister(Object subscriber) {
    b.unregister(subscriber);
  }

  public static void post(Object event) {
    log.debug("Posting: {}", event);
    b.post(event);
  }

  public static void cancelEventDelivery(Object event) {
    b.cancelEventDelivery(event);
  }

  public static void postSticky(Object event) {
    log.debug("Posting sticky: {}", event);
    b.postSticky(event);
  }

  public static <T> T getStickyEvent(Class<T> eventType) {
    return b.getStickyEvent(eventType);
  }

  public static <T> T removeStickyEvent(Class<T> eventType) {
    return b.removeStickyEvent(eventType);
  }

  public static boolean removeStickyEvent(Object event) {
    return b.removeStickyEvent(event);
  }

  public static void removeAllStickyEvents() {
    b.removeAllStickyEvents();
  }

  public static boolean hasSubscriberForEvent(Class<?> eventClass) {
    return b.hasSubscriberForEvent(eventClass);
  }
}
