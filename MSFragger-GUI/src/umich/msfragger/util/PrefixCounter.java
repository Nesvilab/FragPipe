/*
 * Copyright 2018 Dmitry Avtonomov.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package umich.msfragger.util;

import java.util.ArrayDeque;
import java.util.Map;
import java.util.TreeMap;

/**
 *
 * @author Dmitry Avtonomov
 */
public class PrefixCounter {
    
    public enum Mode {FWD, REV};
    
    protected Node root;
    public final Mode mode;
    public final int maxDepth;
    
    public PrefixCounter(Mode mode, int maxDepth) {
        this.mode = mode;
        this.maxDepth = maxDepth;
        this.root = new Node('\uFFFF', 0); // \uFFFF is 'not a character'
    }
    
    public class Node {
        TreeMap<Character, Node> map;
        public final char ch;
        public final int depth;
        private long hits;

        public Node(char ch, int depth) {
            this.ch = ch;
            this.depth = depth;
            map = new TreeMap<>();
            hits = 0;
        }

        @Override
        public String toString() {
            return "Node{" + "ch=" + ch + ", depth=" + depth + ", size=" + map.size() + ", hits=" + hits + '}';
        }
        
        public long getHits() {
            return hits;
        }
    }
    
    public void add(CharSequence csq) {
        switch (mode) {
            case FWD: {
                int pos = -1;
                int len = csq.length();
                Node n = root;
                n.hits++;
                while (++pos < len) {
                    char c = csq.charAt(pos);
                    Node nn = n.map.get(c);
                    if (nn == null) {
                        nn = new Node(c, n.depth + 1);
                        n.map.put(c, nn);
                    }
                    if (nn.depth > maxDepth)
                        return;
                    nn.hits++;
                }
                
                
                break;
            }
            
            case REV: {
                int pos = csq.length();
                Node n = root;
                n.hits++;
                while (--pos >= 0) {
                    char c = csq.charAt(pos);
                    Node nn = n.map.get(c);
                    if (nn == null) {
                        nn = new Node(c, n.depth + 1);
                        n.map.put(c, nn);
                    }
                    if (nn.depth > maxDepth)
                        return;
                    nn.hits++;
                }
                break;
            }
        }
    }
    
    public void iterPrefixCounts(int maxDepth, rx.functions.Action1<Node> action) {
        ArrayDeque<Node> fifo = new ArrayDeque<>();
        fifo.add(root);
        while (!fifo.isEmpty()) {
            Node head = fifo.removeFirst();
            if (head.depth > maxDepth)
                break;
            for (Map.Entry<Character, Node> e : head.map.entrySet()) {
                fifo.addLast(e.getValue());
            }
            action.call(head);
        }
    }
    
    public void printPrefixCounts(int maxDepth) {
        ArrayDeque<Node> fifo = new ArrayDeque<>();
        fifo.add(root);
        while (!fifo.isEmpty()) {
            Node head = fifo.removeFirst();
            if (head.depth > maxDepth)
                break;
            for (Map.Entry<Character, Node> e : head.map.entrySet()) {
                fifo.addLast(e.getValue());
            }
            System.out.println(head);
        }
    }
}
