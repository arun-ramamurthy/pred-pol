#!/usr/bin/env python
# -*- mode: python; fill-column: 79; comment-column: 50 -*-

#
# Author(s):  KL
# Maintainer: PB
# Created:    20160815
# License:    (c) HRDAG, GPL-v2 or greater
# ============================================
#
# todo:
#

import numpy as np
import copy


def lower_tri_ij(dim):
    ''' tril_indices makes lower-triangular indexing easy '''
    ii, jj = np.tril_indices(dim)
    for i, j in zip(ii, jj):
        yield i, j


def lambdafun(t, mu, theta, omega):
    days_til_last = [(t[-1] - ti).days for ti in t[:-1]]
    epart = sum([np.exp(-omega * d) for d in days_til_last])
    return mu + theta * omega * epart


def calc_tij(data):
    assert all([len(n) > 0 for n in data.values()])
    tij = dict()
    for n, events in data.items():
        tij_len = len(events) - 1
        tij[n] = np.zeros((tij_len, tij_len))
        for i, j in lower_tri_ij(tij_len):
            td = (events[i + 1] - events[j]).days
            tij[n][i, j] = td
    return tij


def calc_pij(tij, theta, omega):
    pij = dict()
    for n in tij:
        pij[n] = np.zeros(tij[n].shape)
        for i, j in lower_tri_ij(tij[n].shape[0]):
            if tij[n][i, j] > 0:
                e_part = np.exp(-omega * tij[n][i, j])
                pij[n][i, j] = e_part * theta * omega
    return pij


def estep(data, mu, theta, omega, tij):
    # t these asserts are fast and document the common structure
    assert data.keys() == mu.keys()
    assert data.keys() == tij.keys()
    pj = dict()
    pij = calc_pij(tij, theta, omega)
    for n in data:
        # should possibly append a 1 to the front of this
        denom = mu[n] + pij[n].sum(axis=1)
        pj[n] = mu[n] / denom
        pij[n] = pij[n] / denom
        pj[n] = np.append(pj[n], 1)
    return pij, pj


# t iterating over a dict means over keys unless otherwise specified
def mstep(pij, pj, tij, data, T):
    # t these asserts are fast and document the common structure
    assert data.keys() == pij.keys()
    assert data.keys() == tij.keys()
    assert data.keys() == pj.keys()
    total_events = sum([len(v) for v in data.values()])
    # double sum: arrays over bins; np.sum whole array at once
    sum_pijs = sum([np.sum(pij[n]) for n in pij])
    denom = sum([np.sum(pij[n] * tij[n]) for n in pij])
    omega = sum_pijs / denom
    theta = sum_pijs / total_events
    mu = dict((n, sum(pj[n]) / T) for n in pj)
    # mu = np.ones(num_bins)*sum(mu)  #this is what the paper says to do but
    # this forces the "background rate" to be the same everywhere,
    return omega, theta, mu


def runEM(data, T, pred_date, k=20,
          theta_init=1, omega_init=1, mu_init=1,
          tol1=.00001, tol2=.00001, tol3=.0001):
    num_bins = len(data)
    theta = theta_init
    mu = dict((key, mu_init) for key in data)
    omega = omega_init
    omega_last = 10 + omega
    theta_last = 10 + theta
    mu_last = dict((key, mu_init + 10) for key in data)
    k = min(num_bins, k)
    tij = calc_tij(data)

    while(abs(omega - omega_last) > tol1 and
          abs(theta - theta_last) > tol2 and
          sum([abs(mu[n] - mu_last[n]) for n in mu]) > tol3):
        omega_last = omega
        theta_last = theta
        mu_last = copy.deepcopy(mu)
        pij, pj = estep(data, mu, theta, omega, tij)
        omega, theta, mu = mstep(pij, pj, tij, data, T)

        # deprecate?
        # I did this when I was debugging so that it wouldn't run away
        if omega > T * 1000:
            omega = omega_last
        # assert omega < T * 1000

    # get conditional intensity for selected parameters
    # need to add on latest date
    for n in data:
        data[n] = np.append(data[n], pred_date)

    # todo(KL): everything else is a dict[n], shouldn't rates be a dict?
    rates = [lambdafun(data[n], mu[n], theta, omega) for n in data]
    sorted_keys = [key for (rate, key)
                   in sorted(zip(rates, data), reverse=True)]
    return rates, sorted_keys[0:k], omega, theta


if __name__ == '__main__':
    # the informal tests have been moved to test_predpol.py
    # to find them, search git log for 'informal tests removed'
    pass
